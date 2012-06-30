# myplot.R

# (6/30 changed runif(blahblah) to runif(1) in rgb() calls.
# runif(blahblah) where blahblah > 1 makes multiple random numbers,
# which was making multiple rgb numbers.  These would get used to
# color distinct points if I was plotting points as such, and I
# wouldn't want them to have distinct colors anyway.)

# THIS IS APPARENTLY THE RIGHT WAY TO GET THE VARIANCE OF A ROW:
# rowVars(d75[2,])
#        2 
# 0.1506642 
# that last number is the answer


# global definition of domain plot labels. 	
domain.labels=data.frame(H="hunting", "P"="parenting", OE="earth origin", OS="sky origin")

# make a sequence of colors to be used by all plotting functions
maxcolors = 40000  # should be more than the number of lines we might plot
mycolors = rgb(runif(maxcolors),runif(maxcolors),runif(maxcolors))

# NOTE: R's built-in var and sd are sample variance and standard dev.
# I should actually multiply them by N/(N-1).  The problem is that I'm not
# confident that I always know how to determine N.  R is finicky about data
# structures and dimensions.


# plot each column as a timeseries, all on the same plot, with random colors
plotAllActivns <- function(data){
  cols <- length(data)
  rows <- nrow(data)
  # set up empty plot window with limits xlim, ylim
  plot(1, type="n", ylim=c(-1,1), xlim=c(1,rows), ylab="activation", xlab="time")
  for(i in 1:cols){
    lines(data[i], type="l", col=mycolors[i])
  }
}


# return all activations for propositions of person, in domain, at time tick
findActivns <- function(data, person, domain, tick) {
  # find column names that start with person_domain, return corresponding column indexes
  indexes <- grep(paste0("^", person, "_", domain), colnames(data))
  data[tick, indexes]  # return row passed in parameter tick, from each column referenced by indexes
}

# extract the person names from the data
extractPersons <- function(data) {
  unique(sub("(.*)_.*", "\\1", colnames(data))) # in col names, subst the part before "_" for whole thing, eliminate duplicates:
}

# extract the proposition domains from the data
extractDomains <- function(data) {
  unique(sub(".*_([^.]*)\\..*", "\\1", colnames(data)))  # in col names, subst the part just after "_" for whole thing, eliminate duplicates
}

# a standard dev fn that can be passed to plotForDomain
# NOTE UNVERIFIED but seems like it's working.
# t() is transform, i.e. flip a row to a column
# The apply() stuff is just what R told me to do when I ran sd():
# It said "sd(<matrix>) is deprecated. Use apply(*, 2, sd) instead."
rowSDs <- function(row) {apply(t(row),2,sd)}
rowVars <- function(row) {(rowSDs(row))^2}

# this doesn't work right with plotForDomain, although this
#       rowVars(data[2,])
# works.
#rowVars <- function(row){var(t(row))}

# I haven't figured out how define this functionally, like plotAvgsForDomain and
# plotSDsForDomain below, so I'm just giving it its own definition.
plotActivnsForDomain <- function(data, domain){
  rows <- nrow(data)
  cols <- length(data)

  plot(1, type="n", ylim=c(-1,1), xlim=c(1,rows), ylab="activation", xlab="time", main=domain.labels[1,domain]) # initialize plot window

  for(i in grep(paste0("_", domain, "."), colnames(data))) {
    lines(data[i], type="l", col=mycolors[i])
  }
}

# these work
plotAvgsForDomain <- function(data, domain) {plotForDomain(data, domain, rowMeans, "avg activation")}
plotSDsForDomain <- function(data, domain) {plotForDomain(data, domain, rowSDs, "sd", ymin = 0); print("Warning: Is rowSDs defined correctly?");}
plotVarsForDomain <- function(data, domain) {plotForDomain(data, domain, rowVars, "var", ymin = 0); print("Warning: Is rowVars defined correctly?");}

#plotAvgsForDomain2 <- function(data, domain) {plotForDomain2(data, domain, rowMeans)}
#plotSDsForDomain2 <- function(data, domain) {plotForDomain2(data, domain, rowSDs, ymin = 0)}

# but these don't:
#plotActivnsForDomain <- function(data, domain) {plotForDomain(data, domain, identity)}
#plotVarsForDomain <- function(data, domain) {plotForDomain(data, domain, rowVars, ymin = 0)}

plotForDomain <- function(data, domain, aggregFn, ylabel, ymin = -1, ymax = 1) {
  persons <- extractPersons(data)
  npersons <- length(persons)
  rows <- nrow(data)

  plot(1, type="n", ylim=c(ymin,ymax), xlim=c(1,rows), ylab=ylabel, xlab="time", main=domain.labels[1,domain]) # initialize plot

  color.index = 1
  for (p in persons) {
    lines(aggregFn(findActivns(data, p, domain, )),  # missing tick returns a vector
           type="l", col=mycolors[color.index])
    color.index=color.index+1
  }
}

plotActivnsFourDomains <- function(data, titl) {plotFourDomains(data, plotActivnsForDomain, titl)}
plotAvgsFourDomains <- function(data, titl) {plotFourDomains(data, plotAvgsForDomain, titl)}
plotSDsFourDomains <- function(data, titl) {plotFourDomains(data, plotSDsForDomain, titl)}
plotVarsFourDomains <- function(data, titl) {plotFourDomains(data, plotVarsForDomain, titl)}

plotFourDomains <- function(data, plotFn, titl) {
  # cf. http://sphaerula.com/legacy/R/multiplePlotFigure.html
  
  getOption( "device" )() # open new default device.
  par(mfrow=c(2,2)) # set the mfrow param to 2x2 subplots accessed from left to right, then top to bottom
  # add the four subplots in order :
  plotFn(data, "P")
  plotFn(data, "H")
  plotFn(data, "OE")
  plotFn(data, "OS")

  # print title in outer margin at top, adding a newline as a simple way to shift it down
  title(paste0("\n", titl), outer=TRUE)
}

plotFourDomainsSummary <- function(data, titl) {
  getOption( "device" )() # open new default device.
  par(mfrow=c(2,4)) # set the mfrow param to 2x2 subplots accessed from left to right, then top to bottom
  plotAvgsForDomain(data, "P")
  plotAvgsForDomain(data, "H")
  plotSDsForDomain(data, "P")
  plotSDsForDomain(data, "H")
  plotAvgsForDomain(data, "OE")
  plotAvgsForDomain(data, "OS")
  plotSDsForDomain(data, "OE")
  plotSDsForDomain(data, "OS")
  title(paste0("\n", titl), outer=TRUE) # print title in outer margin at top, adding a newline as a simple way to shift it down
}


# quick and dirty load the file and plot the averages in each domain
loadNplotAvgs <- function(filename) {
  data <- read.csv(filename)
  titl <- sub("PropnData.csv", "", filename, fixed=TRUE) # construct title by extracting basename from filename
  plotAvgsFourDomains(data, titl)
}

# quick and dirty load the file and plot the averages and sd's in each domain
loadNplot <- function(filename) {
  data <- read.csv(filename)
  basetitle <- sub("PropnData.csv", "", filename, fixed=TRUE) # construct title by extracting basename from filename
  plotFourDomainsSummary(data, basetitle)
  plotActivnsFourDomains(data, basetitle)
}
  
# to pop up plots for a lot of files at once, you can do something like this:
# for(i in 1:20){ loadNplotAvgs( paste0("e2sAddNeg0extra1500addl8flippedRun", i, "PropnData.csv") ) }
  
# to pop up plots for a lot of files at once, you can do something like this:
# for(i in 1:20){ loadNplotAvgs( paste0("e2sAddNeg0extra1500addl8flippedRun", i, "PropnData.csv") ) }
