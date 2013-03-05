xyMeanActivnPlot <-
function(form, data, yfoci=seq(-1,1,.2), xfoci=seq(-1,1,.2), auto.key=T, ...){
  require(lattice)
  xyplot(form, data=data, xlim=c(-1,1), ylim=c(-1,1), aspect="iso", auto.key=auto.key,
         abline=c(list(h=yfoci, v=xfoci),             # grid
		  list(a=0,b=1),                      # diagonal
                  trellis.par.get("reference.line")), # color
         ...)  # groups argument might be passed here
}
