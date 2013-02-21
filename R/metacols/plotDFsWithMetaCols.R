#average popcoPlot.R
# Marshall Abrams
# R plotting designed for POPCO simulations

# NOTES ON THE RIGHT WAY TO GET THE VARIANCE OF A ROW:
# rowVars(d75[2,])
#        2 
# 0.1506642 
# that last number is the answer
# NO, BETTER:
# var(as.numeric(d75[2,]))
# or
# convert the data frame to a matrix using:
# m75 <- data.matrix(d75)

#####################################
## GLOBALS USED BY FUNCTIONS BELOW

# make a sequence of colors to be used by all plotting functions
maxcolors = 40000  # should be more than the number of lines we might plot
mycolors = rgb(runif(maxcolors),runif(maxcolors),runif(maxcolors))

# This is a transparent gray for shading part of a plot
bgray <- rgb(190, 190, 190, alpha=80, maxColorValue=255) # alph 0 = fully transparent; 180 = opaque (?)

nonPropnColnames <- c("RUNID", "TICK") # column names that don't rep personal propns
nonPropnColnamesRegexp <- paste0("^(", paste(nonPropnColnames, collapse="|"), ")$")
# NEED TO ADD "^(" and ")$" around this to force match of entire colname

#####################################
## THE FUNCTIONS BELOW ...

# my population variance function that can be applied to rows (!)
pop.var <- function(x){
	n = length(x)
	var(as.numeric(x)) * ((n-1)/n)
}

pop.sd <- function(x){sqrt(pop.var(x))}

# global definition of domain plot labels. 	
domain.labels=data.frame(H="hunting", P="parenting", OE="earth origin", OS="sky origin")


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
  personalPropnNames <- grep(nonPropnColnamesRegexp, colnames(data), value=TRUE, invert=TRUE) # first strip out the non-proposition, meta-info column names
  unique(sub("(.*)_.*", "\\1", personalPropnNames))  # in remaining col names, subst the part BEFORE "_" for whole thing, eliminate duplicates:
}

# extract the proposition domains from the data
extractDomains <- function(data) {
  personalPropnNames <- grep(nonPropnColnamesRegexp, colnames(data), value=TRUE, invert=TRUE) # first strip out the non-proposition, meta-info column names
  unique(sub(".*_([^.]*)\\..*", "\\1", personalPropnNames))  # in remaining col names, subst the part immediately AFTER "_" for whole thing, eliminate duplicates
}

# extract the generic propn names from the data
extractGenericPropns <- function(data) {
  personalPropnNames <- grep(nonPropnColnamesRegexp, colnames(data), value=TRUE, invert=TRUE) # first strip out the non-proposition, meta-info column names
  unique(sub(".*_(.*)", "\\1", personalPropnNames)) # strip off person name and "_"
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

plotActivnsForDomainWithBox <- function(data, domain, boxright){
  rows <- nrow(data)
  cols <- length(data)

  plot(1, type="n", ylim=c(-1,1), xlim=c(1,rows), ylab="activation", xlab="time", main=domain.labels[1,domain]) # initialize plot window

  if (boxright > 0) {
    rect(xleft=0, xright=boxright, ybottom=-1, ytop=1, density=100, col=bgray) # make gray box
  }

  for(i in grep(paste0("_", domain, "."), colnames(data))) {
    lines(data[i], type="l", col=mycolors[i])
  }
}

plotAvgsForDomain <- function(data, domain) {plotForDomain(data, domain, rowMeans, "per-person activation avg")}
# plotVarsForDomain <- function(data, domain) {plotForDomain(data, domain, pop.var, "var", ymin = 0)}
# plotSDsForDomain <- function(data, domain) {plotForDomain(data, domain, pop.sd, "sd", ymin = 0)}
# old versions:
plotSDsForDomain <- function(data, domain) {plotForDomain(data, domain, rowSDs, "per-person activation sd", ymin = 0); print("Warning: Is rowSDs defined correctly?");}
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

plotAvgsForDomainWithBox <- function(data, domain, boxr) {plotForDomainWithBox(data, domain, rowMeans, "per-person average activation values", boxright=boxr)}
plotAvgsFourDomainsWithBoxes <- function(data, titl, boxr) {plotFourDomains(data, function(data, domain){plotAvgsForDomainWithBox(data, domain, boxr)}, titl)}
plotActivnsFourDomainsWithBoxes <- function(data, titl, boxr) {plotFourDomains(data, function(data, domain){plotActivnsForDomainWithBox(data, domain, boxr)}, titl)}

plotForDomainWithBox <- function(data, domain, aggregFn, ylabel, ymin = -1, ymax = 1, boxleft = 0, boxright = 0) {
  persons <- extractPersons(data)
  npersons <- length(persons)
  rows <- nrow(data)

  plot(1, type="n", ylim=c(ymin,ymax), xlim=c(1,rows), ylab=ylabel, xlab="time", main=domain.labels[1,domain]) # initialize plot

  if (boxright > 0) {
    rect(xleft=boxleft, xright=boxright, ybottom=ymin, ytop=ymax, density=100, col=bgray) # make gray box
  }

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
  rawdata <- read.csv(filename)
  data <- rawdata[3:length(rawdata)] # strip runid and tick columns - not needed here
  titl <- sub("PropnData.csv", "", filename, fixed=TRUE) # construct title by extracting basename from filename
  plotAvgsFourDomains(data, titl)
}

# quick and dirty load the file and plot the averages and sd's in each domain
loadNplot <- function(filename) {
  rawdata <- read.csv(filename)
  data <- rawdata[3:length(rawdata)] # strip runid and tick columns - not needed here
  basetitle <- sub("PropnData.csv", "", filename, fixed=TRUE) # construct title by extracting basename from filename
  plotActivnsFourDomains(data, basetitle)
  plotFourDomainsSummary(data, basetitle)
}

# quick and dirty load the file and plot the averages, sd's, and raw activns in each domain
loadNplotSummary <- function(filename) {
  rawdata <- read.csv(filename)
  data <- rawdata[3:length(rawdata)] # strip runid and tick columns - not needed here
  basetitle <- sub("PropnData.csv", "", filename, fixed=TRUE) # construct title by extracting basename from filename
  plotFourDomainsSummary(data, basetitle)
}
