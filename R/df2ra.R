## df2ra.R
## functions for creating arrays from dataframes containing POPCO data.

## EXAMPLE USAGE:
# a <- read2multirunRA(csvs)   # defined below: create multi-run array

# SIMPLE PLOTTING FOR ONE RUN AND PERSON ACROSS TIME:
# plot(1, type="n", ylim=c(-1,1), xlim=c(1,1500), ylab="activation", xlab="time")   # make empty plot w/ activation range (-1,1) and 1500 ticks
# lines(a["S01", "P.WOMAN.CREATES.CHILD.FROM.WITHIN", ,"RUN043839485"], type="p", pch=".", col="blue") # dot-plot one propn in person SO1 in one run
# lines(a["S01", "P.CHILD.CLOSE", ,"RUN043839485"], type="p", pch=".", col="red")                      # dot-plot another propn for same person and run
# lines(apply(a["S01",,,"RUN043839485"], 2, mean), type="l")  # line-plot mean activn at each tick for that person, run
   # notes on this instance of apply: 
   # Subscripts fix the person and run, but let propn and tick vary.
   # The result is a 2D array with dimensions propn, tick.
   # Second arg to apply, i.e. index 2, says: for each tick, get the mean of whatever variation is still allowed.
   # i.e. for each tick, take the mean of all propn activations at that tick.

# EXTRACTION AND COMPARISON OF DOMAINS AT ONE TIME ACROSS RUNS:
# ah <- multiRA2domRA(a, "H")  # defined below: extract subarray for proposition domain H
# ap <- multiRA2domRA(a, "P")
# ah1500 <- ah[,,1500,]        # extract subarrays for timestep 1500
# ap1500 <- ap[,,1500,]
# ah1500means <- apply(ah1500, c(1,3), mean)  # get average activation for each person in each run
# ap1500means <- apply(ap1500, c(1,3), mean)
# ap1500means - ah1500means                   # crude display of difference between H and P activations
# sum(ap1500means - ah1500means)              # even cruder check to see which is greater on average
# ap1500means/ah1500means                     # ratios are also interesting

##############################################################
# file read functions

# Given a list or vector of filenames, return a list of dataframes, one for each input file.
readcsvs <- function(csvs) { lapply(csvs, read.csv) }

# Given a list or vector of filenames, return a list of arrays created by df2RA(), one for each input file.
read2RAs <- function(csvs) { lapply(readcsvs(csvs), df2RA) }

# Given a list or vector of filenames, return a 4-dimensional array created by RAs2multirunRA()
read2multirunRA <- function(csvs) { RAs2multirunRA(read2RAs(csvs), stripcsv(csvs)) }

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
#
# This function assumes that the meta columns have already been stripped by stripMetaCols()

# NOTE the following abstraction of the following definition is not currently needed, but was, and maybe will be.
df2RA <- function(dframe) {strippedDf2RA(dframe)}
# df2RA <- function(dframe) {strippedDf2RA(stripMetaCols(dframe))} NOT CURRENTLY USED

strippedDf2RA <- function(dframe) {
  # extract desired dimensions and labels from the dataframe:
  cols = colnames(dframe)
  persnames = persPropNames2persNames(cols) # ; print(persnames)
  propnames = persPropNames2genPropNames(cols) # ; print(propnames)
  nticks = nrow(dframe) # ; print(nticks)
  ticks = 1:nticks
  npersons = length(persnames)
  npropns = length(propnames)

  # Create version of the array we want, but with the inner matrices
  # defined by first two dimensions flipped along the diagonal.  
  # Seems to be the only easy way to do it.
  # See notes below for further explanation.
  flippedmats = array( t(dframe) ,     c(npropns,   npersons,  nticks) , 
                       dimnames=list(propnames, persnames, ticks) )

  # Now return version with inner (i.e. first two) dimensions swapped,
  # so that the resulting array is what we want:
  aperm(flippedmats, c(2, 1, 3) )
}

##############################################################
# RAs2multirunRA
# Given a list of 3-D arrays of the kind produced by df2RA, and a list
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
  ra[ , getDomainColnums(ra, dom) , , ]  # return an array with only columns we want
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
