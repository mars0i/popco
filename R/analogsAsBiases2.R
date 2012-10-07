# analogsAsBiases2.R
# For processing output from parenting18analogsAsBiases2.lisp and
# associated scripts.

firstTick <- 1500

source("~/popco/R/df2ra.R")

# NOTE: Assumes AA, the "assured advocate", i.e. true
# believer/perceiver, is always the first person.

csvs <- list.files(pattern="*.csv")

#print("Loading data into full multirun array...")
#print("Loading data into multirun array containing only tick 1500...")
#print(system.time(a <- read2multirunRA(csvs, firstTick=1500)))

# better to use steps so we can see how long they take:

print("Loading csv files into list of data frames ...")
print(system.time(dfs <- readcsvs(csvs)))

print("Convert list of data frames into list of arrays ...")
print(system.time(ras <- lapply(dfs, df2RA, firstTick=firstTick)))

print("Load list arrays into a single multi-run array ...")
print(system.time( a <- RAs2multirunRA( ras, stripcsv(csvs) ) ))
# This has dimensions person, proposition, tick, run.

rm(dfs) # clean up to save space
rm(ras) # ditto
# rm(a)

numPundits <- length(grep("^AA", dimnames(a)[[1]], invert=FALSE))

print("Subsetting into P/H-at-tick-1500 arrays (and removing all True Believers)")
ap1500 <- multiRA2domRA(a, "P")[grep("^AA", dimnames(a)[[1]], invert=TRUE), , , ] # true believers are assumed named "AA"-something
ah1500 <- multiRA2domRA(a, "H")[grep("^AA", dimnames(a)[[1]], invert=TRUE), , , ] # i.e. "assured advocates"
# old version:
#ap1500 <- multiRA2domRA(a, "P")[2:dim(a)[1], , , ]
#ah1500 <- multiRA2domRA(a, "H")[2:dim(a)[1], , , ]

#ap1500 <- multiRA2domRA(a, "P")[2:dim(a)[1],,1500,] # combine previous steps
#ah1500 <- multiRA2domRA(a, "H")[2:dim(a)[1],,1500,] # see preceding notes
# NOTE: the "2:dim(a)[1]" says get all rows except the first, which is SP
# These last matrices have dimensions person, proposition, run.

# consider rm'ing a here

# Check that all persons except the true believer have settled into the same state within each run:
cat("Average of per-proposition variances across all runs and persons, for P:", mean(apply(ap1500, c(2), var)), "\n")
cat("Average of per-proposition variances across all runs and persons, for H:", mean(apply(ah1500, c(2), var)), "\n")

print("Making arrays of per person/run average activations")
ap1500m <- apply(ap1500, c(1,3), mean) # for each person and run,
ah1500m <- apply(ah1500, c(1,3), mean) # get average proposition activation
# These matrices have dimensions person, run.

# we use these briefly below:
#rm(ap1500) # cleanup
#rm(ah1500)

# (Or add the check here.  Within each run, each person's average should be
# almost the same.  But the check above is more careful.)
# Here's one way to check:
#sum(apply(ap1500m, c(2), var))
#sum(apply(ah1500m, c(2), var))
# Or since the preceding is sensitive to number of persons, you could do:

cat("Average of run variances for P:", mean(apply(ap1500m, c(2), var)), "\n")
cat("Average of run variances for H:", mean(apply(ah1500m, c(2), var)), "\n")
# If the check succeeds, giving small values, then you can just average all
# of the persons in a run together (so maybe combine steps later):
print("Making per-run average vectors")
ap1500mm <- apply(ap1500m, c(2), mean) # Since within a run, each person as about the same average activation,
ah1500mm <- apply(ah1500m, c(2), mean) # we might as well just use the average across all persons in each run.
# Note these are vectors, with length equal to the number of runs.

print("Ready to plot.")

maxcolors = 40000  # should be more than the number of points we might plot
mycolors = rgb(runif(maxcolors),runif(maxcolors),runif(maxcolors))

# plot all of the H/P averages combinations as points precisely:
plot1500 <- function () {scatter2domains(ah1500mm, ap1500mm)}

pointSize <- 3
addlScatter <- .04
mainTitle <- "Population-wide per-run average activations in two domains,\n"
subTitle  <- "Points shifted to make nearby points visible. Red x = overall mean."

# Same thing, but add some random extra scatter to pull apart nearly identical points:
plot1500plus <- function (biasDesc) {
  main <- paste(mainTitle, length(ap1500mm), " runs with ", biasDesc, " bias (", numPundits, " pundit", if(numPundits>1){"s"}, ")", sep="")
  sub <- subTitle
  # ends up below bottom of window/page:
  #sub <- paste(subTitle, "\nNumber of propositions in hunting domain:", dim(ah1500)[2], "\nNumber of propositions in parenting domain:", dim(ap1500)[2])
  scatter2domains( ah1500mm + runif(ah1500mm, max=addlScatter), ap1500mm + runif(ap1500mm, max=addlScatter), xlab="hunting", ylab="parenting", main)
}

scatter2domains <- function (dom1vals, dom2vals, xlab="domain 1", ylab="domain 2", main=mainTitle, sub=subTitle) {
  npoints <- length(dom1vals)
  mycolors = rgb(runif(npoints),runif(npoints),runif(npoints))
  dev.new() # open new plot window, leave old ones open
  plot(x=dom1vals, y=dom2vals, type="p", pch=".", cex=pointSize, ylim=c(-1,1), xlim=c(-1,1), col=mycolors, xlab=xlab, ylab=ylab, main=main, sub=sub, asp=1)
  lines(x=c(-1,1), y=c(-1,1), col="blue") # draw diagonal line indicating where equal P, H pairs would lie
  lines(x=mean(dom1vals), y=mean(dom2vals), type="p", pch="x", col="red")
}

# if running in batch, automatically plot:
if ( ! interactive() ) {
  plot1500plus(commandArgs(trailingOnly=TRUE)[1])
}
