stripRunPaths <-
function(mra) {
  dimnames(mra)$run <- lapply(dimnames(mra)$run, basename)
  mra
}
