activnsAtTickBarchart <-
function(mra, tick, run=1, main=paste("tick", tick), xlab="activation") {
  require(lattice)

  divadj <- .50 # pushes inter-domain lines up a bit
  # trellgray <- trellis.par.get("reference.line")$col;  # gets default grid gray - lighter than "gray"

  # extract propositions and domain info from data array, and prepare param lists for barchart:
  npersons <- dim(mra)[1]
  propnms <- dimnames(mra)$proposition
  domnms <- genPropNames2domNames(propnms) # get a vector of all domains to which these propns are assigned
  domsizes <- unlist(lapply(domnms, countPropsInDomain, propnms=propnms))  # count how many propns in each domain
  domdivs <- cumsum(domsizes[-length(domsizes)])+divadj  # cumulative sums excluding last element
  domcols <- c(rep("blue", domsizes[1]), rep("darkgreen", domsizes[2]), rep("red", domsizes[3]), rep("darkorange", domsizes[4]))

  barchart(t(mra[,,tick,run]), groups=person, 
           xlim=c(-1,1), 
           scales=list(cex=.5, y = list(alternating = 3)), 
	   xlab=xlab,
	   layout=c(npersons,1),
	   main=main,
           panel = function(y, ...){
             panel.abline(v=c(-.5,.5), lty=3, col="gray");
             panel.abline(h=domdivs, lty=2, col="gray");
             panel.barchart(y=y, col=domcols[y], border="transparent", ...)})
}
