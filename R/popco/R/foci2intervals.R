foci2intervals <-
function(foci){
  dif <- foci[2]-foci[1]
  c(-1, foci[1]-dif/2, foci+dif/2, 1)
}
