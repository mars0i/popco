combineMeanDFsWithBiases <-
function(dfs=NULL, biases=NULL) {
  if (length(dfs) == 0 || length(biases) == 0) {stop("dfs or biases is empty.")}
  if (length(dfs) != length(biases)) {stop("lengths of dfs and biases are not the same.")}

  df <- NULL

  for (i in 1:length(dfs)){
    dfs[[i]]$bias <- biases[[i]] # double brackets works with both lists and vectors; single brackets only for vectors here
  }

  do.call(rbind, dfs)
}
