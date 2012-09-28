## df2ra.R
## functions for creating arrays from dataframes containing POPCO data.

nonPropnColnames <- c("RUNID", "TICK") # column names that don't rep personal propns
nonPropnColnamesRegexp <- paste0("^(", paste(nonPropnColnames, collapse="|"), ")$")

# functions for extracting meaningful labels

colNames2propNames <- function(colnms) {grep(nonPropnColnamesRegexp, colnms, value=TRUE, invert=TRUE)} # strip non-proposition metadata column names

persPropNames2genPropNames <- function(propnms) {unique(sub(".*_", "", propnms))}  # extract unique generic propn names from personal propn names

persPropNames2persNames <- function(propnms) {unique(sub("_.*", "", propnms))} # extract unique person names from personal propn names

genPropNames2domNames  <- function(propnms) {unique(sub("([^.]*)\\..*", "\\1", propnms))} # extract domain names (propn prefixes) from generic propn names

persPropNames2domNames <- function(propnms) {genPropNames2domNames(persPropNames2genPropNames(propnms))} # extract domain names (propn prefixes) from personal propn names

colNames2gPropNames <- function(colnms) {persPropNames2genPropNames(colNames2propNames(colnms))}  # extract unique generic propn names from column names
colNames2persNames <- function(colnms) {persPropNames2persNames(colNames2propNames(colnms))} # extract unique person names from column names
colNames2domNames  <- function(colnms) {persPropNames2domNames(colNames2propNames(colnms))} # extract domain names (propn prefixes) from column names

##############################################################
# df2ra
# Function to create array with dims: person, propn, tick 
# from popco dataframe created by read.csv.
# i.e. for each tick, there's a personXpropn matrix.
# [When printed in R, what you'll see is a series of tick-indexed matrices
# going down the page, each matrix having person rows and propn cols.]

df2ra <- function(df) {
  # extract desired dimensions and labels from the dataframe:
  cols = colnames(df)
  persnames = colNames2persNames(cols) # ; print(persnames)
  propnames = colNames2gPropNames(cols) # ; print(propnames)
  nticks = nrow(df) # ; print(nticks)
  ticks = 1:nticks
  npersons = length(persnames)
  npropns = length(propnames)

  # Create version of the array we want, but with the inner matrices
  # defined by first two dimensions flipped along the diagonal.  
  # Seems to be the only easy way to do it.
  flippedmats = array( t(df) ,     c(npropns,   npersons,  nticks) , 
                       dimnames=list(propnames, persnames, ticks) )

  # Now return version with inner (i.e. first two) dimensions swapped,
  # so that the resulting array is what we want:
  aperm(flippedmats, c(2, 1, 3) )
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
# It seems odd to have to flip the df using t, and then flip the inner
# matrices later using aperm, but that's the only way I've figured out
# to do it.  It obviously could be done in one step, but it's easier to
# read in two steps.  Actually, the whole thing could be done in one line.
# The reason for the transform and then permutation is that array() insists
# on reading in colum,row order, but I want my data read in row,column order.
# So I accomodate array()'s wishes to get the data into an array, and then undo 
# the dimensional damage after the fact.
##############################################################
