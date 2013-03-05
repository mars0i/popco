stripbg <-
function(colorstring){
  sb <- trellis.par.get("strip.background") 
  sb[["col"]][1] <- colorstring
  trellis.par.set("strip.background", sb)
}
