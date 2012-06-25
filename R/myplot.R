
# plot each column as a timeseries, all on the same plot, with random colors
plotCols <- function(data){
  cols <- length(data)
  rows <- nrow(data)

  # set up empty plot window with limits xlim, ylim
  plot(1, type="n", ylim=c(-1,1), xlim=c(1,rows))

  for(i in 1:cols){
    lines(data[i], type="l", col=rgb(runif(cols), runif(cols), runif(cols)))
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
  unique(sub(".*_([^.]*)\\..*", "\\1", colnames(data)))  # in col names, subst the part just after "_" for whole thing, eliminate duplicates:
}

# a standard dev fn that can be passed to plotForDomain
# NOTE UNVERIFIED but seems like it's working.
# t() is transform, i.e. flip a row to a column
# The apply() stuff is just what R told me to do when I ran sd():
# It said "sd(<matrix>) is deprecated. Use apply(*, 2, sd) instead."
rowSDs <- function(row){apply(t(row),2,sd)}

# this doesn't work right with plotForDomain, although this
#       rowVars(data[2,])
# works.
rowVars <- function(row){var(t(row))}

# these work
plotAvgsForDomain <- function(data, domain) {plotForDomain(data, domain, rowMeans)}
plotSDsForDomain <- function(data, domain) {plotForDomain(data, domain, rowSDs, ymin = 0)}

# but these don't:
plotColsForDomain <- function(data, domain) {plotForDomain(data, domain, identity)}
plotVarsForDomain <- function(data, domain) {plotForDomain(data, domain, rowVars, ymin = 0)}

plotForDomain <- function(data, domain, aggregFn, ymin = -1, ymax = 1) {
  persons <- extractPersons(data)
  npersons <- length(persons)
  rows <- nrow(data)

  plot(1, type="n", ylim=c(ymin,ymax), xlim=c(1,rows), main=domain) # initialize plot

  for (p in persons) {
    lines(aggregFn(findActivns(data, p, domain, )),  # missing tick returns a vector
           type="l", col=rgb(runif(npersons), runif(npersons), runif(npersons)))
  }
}

plotAvgsFourDomains <- function(data, titl) {plotFourDomains(data, rowMeans, titl)}
plotSDsFourDomains <- function(data, titl) {plotFourDomains(data, rowSDs, titl)}

plotFourDomains <- function(data, aggregFn, titl) {
  # cf. http://sphaerula.com/legacy/R/multiplePlotFigure.html
  
  getOption( "device" )() # open new default device.
  par(mfrow=c(2,2)) # set the mfrow param to 2x2 subplots accessed from left to right, then top to bottom
  # add the four subplots in order :
  plotForDomain(data, "P", aggregFn)
  plotForDomain(data, "H", aggregFn)
  plotForDomain(data, "OE", aggregFn)
  plotForDomain(data, "OS", aggregFn)

  # print title in outer margin at top, adding a newline as a simple way to shift it down
  title(paste0("\n", titl), outer=TRUE)
}

# quick and dirty load the file and plot the averages in each domain
loadNplotAvgs <- function(filename) {
  data <- read.csv(filename)
  titl <- sub("PropnData.csv", "", filename, fixed=TRUE) # construct title by extracting basename from filename
  plotAvgsFourDomains(data, titl)
}
  
# to pop up plots for a lot of files at once, you can do something like this:
# for(i in 1:20){ loadNplotAvgs( paste0("e2sAddNeg0extra1500addl8flippedRun", i, "PropnData.csv") ) }
