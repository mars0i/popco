removePersons <-
function(multiRA, personPrefix) {
  multiRA[grep(paste0("^", personPrefix), dimnames(multiRA)[[1]], invert=TRUE),,, , drop=FALSE]
}
