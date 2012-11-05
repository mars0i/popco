# crime1basicCheck1.R

# First CREATE 4-D MULTIRUN ARRAY FROM LIST/ARRAY OF CSV FILENAMES:
# mra <- read2multirunRA(csvs) 

library(lattice)

# activnsAtTickBarchart:
# Return barcharts of activations for each proposition grouped by person
# using 4-D array mra of popco data
# at tick tick
# with the remaining arguments specifying the number of propositions in each domain, 
# sorted alphabetically.

activnsAtTickBarchart <- function(mra, tick, dom1Npropns, dom2Npropns, dom3Npropns, dom4Npropns, run=1) {
  skosh=.45
  domdivs=c(dom1Npropns,dom1Npropns+dom2Npropns,dom1Npropns+dom2Npropns+dom3Npropns)+skosh
  domcols <- c(rep("blue", dom1Npropns), rep("darkgreen", dom2Npropns), rep("red", dom3Npropns), rep("darkorange", dom4Npropns))
  trellgray <- trellis.par.get("reference.line")$col;  # gets default grid gray - lighter than "gray"

  barchart(t(mra[,,tick,run]), groups=person, 
           xlim=c(-1,1), 
           scales=list(cex=.5, y = list(alternating = 3)), 
	   layout=c(4,1),
           panel = function(y, ...){
             panel.abline(v=c(-.5,.5), lty=3, col=trellgray);
             panel.abline(h=domdivs, lty=2, col="gray");
             panel.barchart(y=y, col=domcols[y], border="transparent", ...)})
}
