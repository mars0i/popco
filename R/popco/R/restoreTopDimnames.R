restoreTopDimnames <-
function(mra) {
  dimnames(mra) <- list(person=dimnames(mra)[[1]], 
                        proposition=dimnames(mra)[[2]], 
                        tick=dimnames(mra)[[3]], 
                        run=dimnames(mra)[[4]])
  mra
}
