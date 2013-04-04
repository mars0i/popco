
apply2panels <- function(f, x, ...) {
  cat("in apply2panels\n")
  panel.locs <- trellis.currentLayout()
  print(panel.locs)
  for (row in 1:nrow(panel.locs))
    for (column in 1:ncol(panel.locs))
       if (panel.locs[row, column] > 0) {
	 cat("row:",row,"column:",column,"\n")
         trellis.focus("panel", row = row, column = column)
	 f(x, ...)
	 trellis.unfocus()
	 cat("after trellis.focus... row:",row,"column:",column,"\n")
  }
}

bwplot(var1 ~ cat | cat2, data=df, pch="|", coef=0, outer=TRUE, panel=function(...){panel.bwplot(...); apply2panels(function(p){panel.points(x=p, pch="x", cex=2, col="red")}, c(.5,-.5))})
