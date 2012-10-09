## df2ra.R
## functions for creating arrays from dataframes containing POPCO data.

## EXAMPLE USAGE:
# a <- read2multirunRA(csvs)   # defined below: create multi-run array
# EXTRACTION AND COMPARISON OF DOMAINS AT ONE TIME ACROSS RUNS:
# ah <- multiRA2domRA(a, "H")  # defined below: extract subarray for proposition domain H
# ap <- multiRA2domRA(a, "P")  # note this function works only on 4D arrays, not lower-dimensional subarrays
# ah1500 <- ah[,,1500,]        # extract subarrays for timestep 1500
# ap1500 <- ap[,,1500,]
# ah1500means <- apply(ah1500, c(1,3), mean)  # get average activation for each person in each run
# ap1500means <- apply(ap1500, c(1,3), mean)
# ap1500means - ah1500means                   # crude display of difference between H and P activations
# sum(ap1500means - ah1500means)              # even cruder check to see which is greater on average
# ap1500means/ah1500means                     # ratios are also interesting

##############################################################

##############################################################
# file read functions

# Return list of all files *.csv in the current working directory.  (Use setwd() to change dir)
getcsvnames <- function() {list.files(pattern="*.csv")}

# Just a wrapper around read.csv to allow adding print statements, etc.
readcsv <- function(csv) {
  cat("Reading ", csv, ". ")
  read.csv(csv)
}

# Given a list or vector of filenames, return a list of dataframes, one for each input file.
readcsvs <- function(csvs) { lapply(csvs, readcsv) }

# Given a list or vector of filenames, return a list of arrays created by df2RA(), one for each input file.
read2RAs <- function(csvs, firstTick=1) {
  lapply(readcsvs(csvs), df2RA, firstTick=firstTick)
}

# Given a list or vector of filenames, return a 4-dimensional array created by RAs2multirunRA()
read2multirunRA <- function(csvs, firstTick=1) {
  RAs2multirunRA(read2RAs(csvs, firstTick=firstTick), stripcsv(csvs))
}

# run read2multirunRA in specified directory, returning to current directory when done:
read2multirunRAfromDir <- function(datadir, firstTick=1) {
  currdir <- getwd()
  setwd(datadir)
  multiRA <- read2multirunRA(getcsvnames(), firstTick)
  setwd(currdir)
  multiRA
}


punditPrefix <- "AA"

# given a multi-run array and two propn domain name strings, strip pundits and return a list containing domain-specific arrays and number of pundits
multirunRA2domRAs <- function(multiRA, dom1, dom2) {
  numPundits <- length(grep("^AA", dimnames(multiRA)[[1]], invert=FALSE))
  dom1RA <- removePersons(multiRA2domRA(multiRA, dom1), punditPrefix)
  dom2RA <- removePersons(multiRA2domRA(multiRA, dom2), punditPrefix)
  #numPundits <- length(grep("^AA", dimnames(multiRA)[[1]], invert=FALSE))
  #dom1RA <- multiRA2domRA(multiRA, dom1)[grep("^AA", dimnames(multiRA)[[1]], invert=TRUE), , , ] # true believers are assumed named "AA"-something
  #dom2RA <- multiRA2domRA(multiRA, dom2)[grep("^AA", dimnames(multiRA)[[1]], invert=TRUE), , , ] # i.e. "assured advocates"
  list(dom1RA, dom2RA, numPundits)
}

removePersons <- function(multiRA, personPrefix) {
  multiRA[grep(paste0("^", personPrefix), dimnames(multiRA)[[1]], invert=TRUE),,,]
}


##############################################################
# NOT CURRENTLY USED:
# these definitions must be coordinated with each other and with POPCO
# stripMetaCols <- function(dframe) {dframe[3:length(dframe)]} # return dataframe without the meta columns
# metaColnames <- c("RUNID", "TICK") # column names that don't rep personal propns
# metaColnamesRegexp <- paste0("^(", paste(metaColnames, collapse="|"), ")$") # regexp that will find the meta col names

##############################################################
# functions for extracting meaningful labels

# these extract various parts of proposition names
persPropNames2genPropNames <- function(propnms) {unique(sub(".*_", "", propnms))}  # extract unique generic propn names from personal propn names
persPropNames2persNames <- function(propnms) {unique(sub("_.*", "", propnms))} # extract unique person names from personal propn names
genPropNames2domNames  <- function(propnms) {unique(sub("([^.]*)\\..*", "\\1", propnms))} # extract domain names (propn prefixes) from generic propn names
persPropNames2domNames <- function(propnms) {genPropNames2domNames(persPropNames2genPropNames(propnms))} # extract domain names (propn prefixes) from personal propn names

# NOTE the following abstractions of the preceding definitions are not currently needed, but were, and maybe will be.
# these can be used to extract the same information if the meta colnames are mixed in with the propn names
# allColNames2propNames <- function(colnms) {grep(metaColnamesRegexp, colnms, value=TRUE, invert=TRUE)} # strip non-proposition metadata column names NOT CURRENTLY USED
allColNames2propNames <- function(colnms) {colnms} # no-op; might be replaced later with something like the preceding
allColNames2gPropNames <- function(colnms) {persPropNames2genPropNames(allColNames2propNames(colnms))}  # extract unique generic propn names from column names
allColNames2persNames <- function(colnms) {persPropNames2persNames(allColNames2propNames(colnms))} # extract unique person names from column names
allColNames2domNames  <- function(colnms) {persPropNames2domNames(allColNames2propNames(colnms))} # extract domain names (propn prefixes) from column names

stripcsv <- function(filenames) {gsub("\\.csv$", "", filenames)} # given vec or list of filenames, return vec of strings with ".csv" removed from end

##############################################################
# df2RA
# Function to create array with dims: person, propn, tick 
# from popco dataframe created by read.csv.
# i.e. for each tick, there's a personXpropn matrix.
# [When printed in R, what you'll see is a series of tick-indexed matrices
# going down the page, each matrix having person rows and propn cols.]
# Arguments:
# dframe: a data frame to use as input data
# firstTick: if passed, the first tick of the dataframe to include (defaults to 1)
# lastTick: if passed, the last tick of the dataframe to include (defaults to the number of rows in the dataframe)
# i.e. the returned RA will go from firstTick to lastTick, inclusive.

df2RA <- function(dframe, firstTick=1, lastTick=nrow(dframe)) {
  print(paste0("converting dframe to array, using ticks", firstTick, " to ", lastTick))
  # extract desired dimensions and labels from the dataframe:
  cols = colnames(dframe)
  persnames = persPropNames2persNames(cols) # ; print(persnames)
  propnames = persPropNames2genPropNames(cols) # ; print(propnames)
  ticks = firstTick:lastTick
  nticks = length(ticks)
  npersons = length(persnames)
  npropns = length(propnames)

  # Create version of the array we want, but with the inner matrices
  # defined by first two dimensions flipped along the diagonal.  
  # Seems to be the only easy way to do it.
  # See notes below for further explanation.
  flippedmats = array( t(dframe[firstTick:lastTick,]) ,     c(npropns,   npersons,  nticks) , 
                       dimnames=list(propnames, persnames, ticks) )

  # Now return version with inner (i.e. first two) dimensions swapped,
  # so that the resulting array is what we want:
  aperm(flippedmats, c(2, 1, 3) )
}

##############################################################
# RAs2multirunRA
# Given a list of 3-D arrays, each of the kind produced by df2RA, and a list
# of run names of the runs which generated the data for those arrays,
# return a 4-D array in which each element along the 4th dimension is
# one of the original 3-D arrays, in order.
# Note: It appears that the arguments can be vectors or lists.

RAs2multirunRA <- function(RAs, runIDs) {
  numRAs = length(RAs)

  # error checking
  if (numRAs != length(runIDs)) { stop("RAs and runIDs have different lengths.") }
  # ideally we could also check that dimensions and dimnames are all same

  RAsInOneVec <- mapply(c, unlist(RAs)) # squash all data from arrays into a single vector

  newDims <- c(dim(RAs[[1]]), numRAs) # construct dimensions of output array - assumes all arrays same
  newDimnames <- dimnames(RAs[[1]]) # again, assuming all arrays are same
  newDimnames[[4]] <- runIDs # extend list of lists of dimnames to include the run ids as names along 4th dimension

  # construct a 4-D array containing each member of RAs as one element along 4th dim:
  array(RAsInOneVec, newDims, newDimnames)
}

##############################################################
# RA2domRA
# Extract an array corresponding to a domain of belief from
# an array containing all beliefs.
# The second argument, dom, is a string for a POPCO proposition 
# prefix representing a domain of belief.

# for single run array
RA2domRA <- function(ra, dom) {
  ra[ , getDomainColnums(ra, dom) , ]  # return an array with only columns we want
}

# for multi-run array
multiRA2domRA <- function(ra, dom) {
  ra[ , getDomainColnums(ra, dom) , , , drop=FALSE]  # return an array with only columns we want, keeping all dims even if length=1
}

# Given an array and a domain string, returns the indexes
# of colums withpropositions from that domain.
getDomainColnums <- function(ra, dom) {
  propnames = dimnames(ra)[2][[1]] # get the proposition names from the ra
  regx = paste0("^", dom, ".")     # we'll search for this string
  grep(regx, propnames)  # get indexes of columns we want
}

##############################################################
# Some notes on how strippedDf2RA works: 
# It seems odd to have to flip the dframe using t, and then flip the inner
# matrices later using aperm, but that's the only way I've figured out
# to do it.  It obviously could be done in one step, but it's easier to
# read in two steps.  Actually, the whole thing could be done in one line.
# The reason for the transform and then permutation is that array() insists
# on reading in colum,row order, but I want my data read in row,column order.
# So I accomodate array()'s wishes to get the data into an array, and then undo 
# the dimensional damage after the fact.
##############################################################
