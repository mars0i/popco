multiRA2domRA <-
function(ra, dom) {
  ra[ , getDomainColnums(ra, dom) , , , drop=FALSE]  # return an array with only columns we want, keeping all dims even if length=1
}
