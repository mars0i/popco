plotPairwiseDiffs <-
function(mra, ymax=2, ymin=-2, type="l", pch=".", jitter=0) {
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
