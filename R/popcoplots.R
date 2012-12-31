# popcoplots.R

# First CREATE 4-D MULTIRUN ARRAY FROM LIST/ARRAY OF CSV FILENAMES:
# mra <- read2multirunRA(csvs) 

# activnsAtTickBarchart:
# Return barcharts of activations for each proposition grouped by person
# using 4-D array mra of popco data
# at tick
# this version assumes 4 domains
activnsAtTickBarchart <- function(mra, tick, run=1, main=paste("tick", tick)) {
  require(lattice)

  divadj <- .50 # pushes inter-domain lines up a bit
  # trellgray <- trellis.par.get("reference.line")$col;  # gets default grid gray - lighter than "gray"

  # extract propositions and domain info from data array, and prepare param lists for barchart:
  propnms <- dimnames(mra)$proposition
  domnms <- genPropNames2domNames(propnms) # get a vector of all domains to which these propns are assigned
  domsizes <- unlist(lapply(domnms, countPropsInDomain, propnms=propnms))  # count how many propns in each domain
  domdivs <- cumsum(domsizes[-length(domsizes)])+divadj  # cumulative sums excluding last element
  domcols <- c(rep("blue", domsizes[1]), rep("darkgreen", domsizes[2]), rep("red", domsizes[3]), rep("darkorange", domsizes[4]))

  barchart(t(mra[,,tick,run]), groups=person, 
           xlim=c(-1,1), 
           scales=list(cex=.5, y = list(alternating = 3)), 
	   xlab="activation",
	   layout=c(4,1),
	   main=main,
           panel = function(y, ...){
             panel.abline(v=c(-.5,.5), lty=3, col="gray");
             panel.abline(h=domdivs, lty=2, col="gray");
             panel.barchart(y=y, col=domcols[y], border="transparent", ...)})
}


# Plot max and min differences between all possible pairs of runs, summarizing how they diverge (or don't)
# parameters:
# mra:		a popco multi-run array. e.g. mra[,,,4:6] will plot differences between runs with indexes 4, 5, and 6.
# ymin, ymin:	min and max y-values allowed in the plot. e.g. make these small to focus in on micro-variation.
# type:		whether to plot smooth line, points, etc. used as value of parameter 'type' in line().
# pch:		for type="p", determines what symbols are plotted; also passed to line().
# jitter: 	if non-zero, a random number from a Gaussian distribution are added to each line.  This allow seeing
#         	overlapping lines.  The Gaussian dist has mean 0, and standard deviation equal to the value of jitter.
#		jitter = .01 or .02 is probably a good choice.
# Note: Uses regular R graphics, not Lattice or ggplot2.
plotPairwiseDiffs <- function(mra, ymax=2, ymin=-2, type="l", pch=".", jitter=0) {
	# list of possible plotting colors:
	plotcolors <- c("black", "blue", "red", "purple", "green", "darkorange", "darkgreen", "navyblue")

	# these will be used to construct a color key label:
	colorsep <- "   "  # this will separate specific color key strings
	colorkey <- ""     # we'll add text to this inside the loop below

	numruns <- dim(mra)[4]
	numticks <- dim(mra)[3]

	# set up plot window:
	plot(0, type="l", 
		xlim=c(1, numticks),
		ylim=c(ymin, ymax),	# max poss variation is 1 minus -1
		xlab=paste(sub(".*/", "", dimnames(mra)$run), collapse=", "), # if run names have paths included, strip them off
		main="max and min differences between runs")

	abline(h=0, col="gray", lty=3)  # add reference line at y=0. lty=2 for dashed, 3 for dotted

	runpairs <- expand.grid(a=1:numruns, b=1:numruns) # get all ordered pairs of run indexes
	runpairs <- runpairs[runpairs$a > runpairs$b, ]   # strip unordered duplicate rows and rows with two of the same index

	for (i in 1:dim(runpairs)[1]) {
		runA <- runpairs[i,1]
		runB <- runpairs[i,2]

		diffs1 <- mra[,,,runA]-mra[,,,runB]
		diffs2 <- mra[,,,runB]-mra[,,,runA]

		# Make sure that the upper plot goes farther up than the lower one goes down.
		# That way, similar plots in different loop iterations will get overlaid instead of appearing to be mirrored.
		if (max(diffs1) > max(diffs2)) {
			diffs <- diffs1
			run1 <- runA
			run2 <- runB
		} else {
			diffs <- diffs2
			run1 <- runB
			run2 <- runA
		}

		if (jitter != 0) {
			diffs <- diffs + rnorm(1, sd=jitter)
		}

		colorkey <- paste0(colorkey, plotcolors[i], ": ", run1, "-", run2, colorsep)

		lines(apply(diffs, 3, max), type=type, pch=pch, col=plotcolors[i])
		lines(apply(diffs, 3, min), type=type, pch=pch, col=plotcolors[i])
	}

	colorkey <- substr(colorkey, 1, nchar(colorkey)-nchar(colorsep)) # strip off the last, extra colorsep
	mtext(colorkey, 1) # 1 on bottom, 4 on the right side
}
