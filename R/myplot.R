
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
plotSDsForDomain <- function(data, domain) {plotForDomain(data, domain, rowSDs)}

# but these don't:
plotColsForDomain <- function(data, domain) {plotForDomain(data, domain, identity)}
plotVarsForDomain <- function(data, domain) {plotForDomain(data, domain, rowVars)}

plotForDomain <- function(data, domain, aggregFn) {
  persons <- extractPersons(data)
  npersons <- length(persons)
  rows <- nrow(data)

  plot(1, type="n", ylim=c(-1,1), xlim=c(1,rows)) # initialize plot

  for (p in persons) {
    lines(aggregFn(findActivns(data, p, domain, )),  # missing tick returns a vector
           type="l", col=rgb(runif(npersons), runif(npersons), runif(npersons)))
  }
}
