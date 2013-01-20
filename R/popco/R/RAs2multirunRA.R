RAs2multirunRA <-
function(RAs, runIDs) {
  cat("converting arrays to mulitrun array\n")
  numRAs = length(RAs)

  # error checking
  if (numRAs != length(runIDs)) { stop("RAs and runIDs have different lengths.") }
  # ideally we could also check that dimensions and dimnames are all same

  RAsInOneVec <- mapply(c, unlist(RAs)) # squash all data from arrays into a single vector

  newDims <- c(dim(RAs[[1]]), numRAs) # construct dimensions of output array - assumes all arrays same
  newDimnames <- dimnames(RAs[[1]]) # again, assuming all arrays are same
  newDimnames[[4]] <- runIDs # extend list of lists of dimnames to include the run ids as names along 4th dimension

  # construct a 4-D array containing each member of RAs as one element along 4th dim:
  newRA <- array(RAsInOneVec, newDims, newDimnames)
  addTopDimNamesToMultiRA(newRA) # returns a modified array
}
