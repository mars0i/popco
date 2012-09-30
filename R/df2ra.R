## df2ra.R
## functions for creating arrays from dataframes containing POPCO data.

# ?:
# ya <- sapply(cs, read.csv)
# df1 <- data.frame(t(matrix(unlist(ya[,1]), ncol=100, byrow=T)))

# Then you can do things like this, I think, to get the average across time for each propn in each person:
# apply(ra, c(1,2), mean)
# or this to get the average at each time for propositions within each person:
# apply(ra, c(1,3), mean)

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
# df2ra
# Function to create array with dims: person, propn, tick 
# from popco dataframe created by read.csv.
# i.e. for each tick, there's a personXpropn matrix.
# [When printed in R, what you'll see is a series of tick-indexed matrices
# going down the page, each matrix having person rows and propn cols.]
#
# This function assumes that the meta columns have already been stripped by stripMetaCols()

# NOTE the following abstraction of the following definition is not currently needed, but was, and maybe will be.
df2ra <- function(dframe) {strippedDf2ra(dframe)}
# df2ra <- function(dframe) {strippedDf2ra(stripMetaCols(dframe))} NOT CURRENTLY USED

strippedDf2ra <- function(dframe) {
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
  flippedmats = array( t(dframe) ,     c(npropns,   npersons,  nticks) , 
                       dimnames=list(propnames, persnames, ticks) )

  # Now return version with inner (i.e. first two) dimensions swapped,
  # so that the resulting array is what we want:
  aperm(flippedmats, c(2, 1, 3) )
}

##############################################################
# RAs2multirunRA
# Given a list of 3-D arrays of the kind produced by df2ra, and a list
# of run names of the runs which generated the data for those arrays,
# return a 4-D array in which each element along the 4th dimension is
# one of the original 3-D arrays, in order.
# Note: It appears that the arguments can be vectors or lists.

RAs2multirunRA <- function(RAs, runIDs) {
  numRAs = length(RAs)

  # error checking
  if (numRAs != length(runIDs)) { stop("RAs and runIDs have different lengths.") }
  # ideally we could also check that dimensions and dimnames are all same

  rasInOneVec <- mapply(c, unlist(RAs)) # squash all data from arrays into a single vector

  newDims <- c(dim(RAs[[1]]), numRAs) # construct dimensions of output array - assumes all arrays same
  newDimnames <- dimnames(RAs[[1]]) # again, assuming all arrays are same
  newDimnames[[4]] <- runIDs # extend list of lists of dimnames to include the run ids as names along 4th dimension

  # construct a 4-D array containing each member of RAs as one element along 4th dim:
  array(rasInOneVec, newDims, newDimnames)
}

##############################################################
# ra2domra
# Extract an array corresponding to a domain of belief from
# an array containing all beliefs.
# The second argument, dom, is a string for a POPCO proposition 
# prefix representing a domain of belief.

ra2domra <- function(ra, dom) {
  propnames = dimnames(ra)[2][[1]] # get the proposition names from the ra
  regx = paste0("^", dom, ".")     # we'll search for this string
  colnums = grep(regx, propnames)  # get indexes of columns we want
  ra[ , colnums , ]  # return an array with only columns we want
}

##############################################################
# Some notes on how df2ra works:
# It seems odd to have to flip the dframe using t, and then flip the inner
# matrices later using aperm, but that's the only way I've figured out
# to do it.  It obviously could be done in one step, but it's easier to
# read in two steps.  Actually, the whole thing could be done in one line.
# The reason for the transform and then permutation is that array() insists
# on reading in colum,row order, but I want my data read in row,column order.
# So I accomodate array()'s wishes to get the data into an array, and then undo 
# the dimensional damage after the fact.
##############################################################
