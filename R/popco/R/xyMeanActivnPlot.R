xyMeanActivnPlot <-
function(form, data, yfoci, xfoci, ...){
  require(lattice)
  xyplot(form, data=data, xlim=c(-1,1), ylim=c(-1,1), aspect="iso", auto.key=T,
         abline=c(list(h=yfoci, v=xfoci),             # grid
		  list(a=0,b=1),                      # diagonal
                  trellis.par.get("reference.line")), # color
         ...)  # groups argument might be passed here
}
