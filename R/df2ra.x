

readcsvs(csvs) {
  dframes <- list()

  for (i in 1:length(csvs)) {
    dframes[[i]] <- read.csv(csvs[i])
  }

  dframes
}

ra2domra <- function(ra, dom) {
  propnames = dimnames(ra)[2][[1]] # get the proposition names from the ra
  regx = paste0("^", dom, ".")     # we'll search for this string
  colnums = grep(regx, propnames)  # get indexes of columns we want
  ra[ , colnums , ]  # return an array with only columns we want
}

# given a multi-run array and two propn domain name strings, strip pundits and return a list containing domain-specific arrays and number of pundits
multirunRA2domRAs <- function(multiRA, dom1, dom2) {
  numPundits <- length(grep("^AA", dimnames(multiRA)[[1]], invert=FALSE))
  dom1RA <- removePersons(multiRA2domRA(multiRA, dom1), punditPrefix)
  dom2RA <- removePersons(multiRA2domRA(multiRA, dom2), punditPrefix)
  #numPundits <- length(grep("^AA", dimnames(multiRA)[[1]], invert=FALSE))
  #dom1RA <- multiRA2domRA(multiRA, dom1)[grep("^AA", dimnames(multiRA)[[1]], invert=TRUE), , , ] # true believers are assumed named "AA"-something
  #dom2RA <- multiRA2domRA(multiRA, dom2)[grep("^AA", dimnames(multiRA)[[1]], invert=TRUE), , , ] # i.e. "assured advocates"
  list(dom1RA, dom2RA, numPundits)
}
