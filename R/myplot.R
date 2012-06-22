
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
  unique(sub(".*_([^.]*)\\..*", "\\1", colnames(tst)))  # in col names, subst the part just after "_" for whole thing, eliminate duplicates:
}

plotAvgs <- function(data, domain) {
  persons <- extractPersons(data)
  npersons <- length(persons)
  rows <- nrow(data)

  plot(1, type="n", ylim=c(-1,1), xlim=c(1,rows))

  for (p in persons) {
    print(p)
    lines( rowMeans(findActivns(data, p, domain, )),  # missing tick returns a vector
           type="l", col=rgb(runif(npersons), runif(npersons), runif(npersons)))
  }
}


#plotAvgs <- function(data) {
#  persons <- extractPersons(data)
#  domains <- extractDomains(data)
#  ...
#}
