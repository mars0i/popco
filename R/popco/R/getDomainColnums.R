getDomainColnums <-
function(ra, dom) {
  propnames = dimnames(ra)[2][[1]] # get the proposition names from the ra
  regx = paste0("^", dom, ".")     # we'll search for this string
  grep(regx, propnames)  # get indexes of columns we want
}
