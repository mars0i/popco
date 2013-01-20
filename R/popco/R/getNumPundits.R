getNumPundits <-
function(multiRA) {
  length(grep(paste0("^", punditPrefix), dimnames(multiRA)[[1]]))
}
