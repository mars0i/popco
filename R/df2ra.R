## df2ra.R
## functions for creating arrays from dataframes containing POPCO data.

## EXAMPLE USAGE:
#
# CREATE 4-D MULTIRUN ARRAY FROM LIST/ARRAY OF CSV FILENAMES:
# mra <- read2multirunRA(csvs)   # defined below: create multi-run array
# SAME THING BUT LOAD AND PROCESS ONE CSV AT A TIME (uses less memory):
# mra <- read2multirunRA(csvs, perload=1)
# SAME THING BUT SKIP FIRST N TICKS:
# read2multirunRAfromDir(datadir, firstTick=1)
# SAME THING BUT PASS DIRECTORY NAME INSTEAD:
# read2dfsFromDir(datadir)
#
# CHECK WHETHER BETWEEN-PERSON ACTIVATIONS HAVE CONVERGED (return names of runs with non-convergent propns):
# findRunsWithDisagreement(mra[-1,,,], tolerance) # don't forget to remove pundits.  defaults to last tick.
# 
# DATAFRAME OF RUN MEANS AT ONE TIME (FOR USE WITH LATTICE). Note pundits will be excluded: 
# df <- multiRA2meanDF(mra, "H", "P") # defaults to last tick only
# Do that twice, and then you can combine the dfs like this:
# df <- combineMeanDFsWithBiases <- function(dfs, biases)
# e.g.
# df <- combineMeanDFsWithBiases <- function(list(dfv, dfb), c("virus", "beast))
#
# Or do it all at once:
# df <- multiRAs2combinedMeanDF(list(mra1, mra2), c("virus", "beast"), "CV", "CB")

#
# EXAMPLES: HOW TO CREATE A LIST OF POSS FOCI TOWARD WHICH RUN MEANS SHOULD CONVERGE:
# xseq <- seq(from=-9, to=9, by=2*.9)/10     # for 10 propns in a domain, max is 10*.9, min 10*-.9
# yseq <- seq(from=-8.1, to=8.1, by=2*.9)/9  # increments between foci are 2*.9
#
# CREATE INTERVALS WITH FOCI AT THEIR MIDPOINTS, FOR BINNING RUN MEANS:
# intervals <- foci2intervals(foci)
#
# DATAFRAME WITH FREQUENCIES IN BINNED RUN MEANS:
# fdf <- DF2freqDF(df, dom1, dom2, dom1intervals, dom2intervals)
#
# EXTRACTION AND COMPARISON OF DOMAINS AT ONE TIME ACROSS RUNS (ARRAYS):
# ah <- multiRA2domRA(mra, "H")  # defined below: extract subarray for proposition domain H
# ap <- multiRA2domRA(mra, "P")  # note this function works only on 4D arrays, not lower-dimensional subarrays
# ah1500 <- ah[,,1500,]        # extract subarrays for timestep 1500
# ap1500 <- ap[,,1500,]
# ah1500means <- apply(ah1500, c(1,3), mean)  # get average activation for each person in each run
# ap1500means <- apply(ap1500, c(1,3), mean)
# ap1500means - ah1500means                   # crude display of difference between H and P activations
# sum(ap1500means - ah1500means)              # even cruder check to see which is greater on average
# ap1500means/ah1500means                     # ratios are also interesting

##############################################################

punditPrefix <- "AA"

#------------------------------------------------------
basename <- function(path) paste(sub(".*/", "", path), collapse=", ")

# usage: mra <- stripRunPaths(mra)
stripRunPaths <- function(mra) {
  dimnames(mra)$run <- lapply(dimnames(mra)$run, basename)
  mra
}

#------------------------------------------------------

# Turn a (probably subsetted) multiple-run RA into a dataframe suitable
# for plotting with xyplot.  Note that this can quickly generate *a lot*
# of rows.  If interval > 1, then sample data every interval ticks to generate the dataframe.
# [have tried rewrite to speed up using data.tables, but they are very finicky.  see df2raDataTable.R]
multiRA2PersonPropnDF <- function(mra, interval=1) {
  dnames <- dimnames(mra)
  persons <- dnames[[1]]
  propns <- dnames[[2]]
  ticks <- dnames[[3]]
  runs <- dnames[[4]]

  domsInPropnOrder<- sub("_.*", "", propns) # list of domain names in the same order as propns they came from

  dims <- dim(mra)
  npersons <- dims[1]
  npropns <- dims[2]
  nticks <- dims[3]
  nruns <- dims[4]

  #cat(npersons, npropns, nticks, runs)
  dummynums <- rep(NA_real_, nticks)
  dummystrings <- rep(NA_character_, nticks)

  # preallocate. sorta.
  newdf <- data.frame(activn=dummynums,
                      person=dummystrings,
                      dom=dummystrings,
                      propn=dummystrings,
                      run=dummystrings,
                      tick=dummynums,
		      stringsAsFactors=FALSE) # otherwise R complains we add new strings

  cat("Making dataframe with", (npersons * npropns * nticks * nruns) / interval, "rows ...\n")

  i <- 0
  for (rn in 1:nruns) {
    for (pr in 1:npersons) {
      for (ppn in 1:npropns) {
        for (tck in seq(1,nticks,interval)) {
	  i <- i + 1 # row index
          newdf[i,] <- c(mra[pr,ppn,tck,rn], persons[pr], domsInPropnOrder[ppn], propns[ppn], runs[rn], tck)
	  if (i %% 1000 == 0) {cat(i, "")} # don't let user get lonely, but don't be obnoxious
        }
      }
    }
  }

  newdf
}

#------------------------------------------------------

# Returns true iff these two dataframes have identical rownames, after possibly removing an extra character added for uniqueness
congruentRuns2dfs <- function(df1, df2) {
  all(substr(dimnames(df1)[[1]],1,12) == substr(dimnames(df2)[[1]],1,12))
}

# Same as above but for two biases in same df
congruentRuns1df <- function(df, bias1, bias2) {
  congruentRuns2dfs(df[df$bias==bias1 && df$rawsum=="raw",], df[df$bias==bias2 && df$rawsum=="raw",])
}

# Subtract runs means wrt one bias from those wrt the other
# the two parts of the dfs must be congruent - i.e. runs in same order
biasDiffs <- function(df, bias1, bias2, doms) {
  newdf <- df[df$rawsum=="raw" && df$bias==bias1, doms] - df[df$rawsum=="raw" && df$bias==bias2, doms]
  newdf$rawsum <- "raw"
  newdf <- rbind(newdf, c(sapply(newdf[,doms], mean), rawsum="mean"))
  newdf[,doms[1]] <- as.numeric(newdf[,doms[1]])  # rbind coerced everything to string by intermediate conversion to matrix
  newdf[,doms[2]] <- as.numeric(newdf[,doms[2]])  # so undo that
  newdf
}

# If you want bias diffs from several different data sets using the same biases and domains,
# it's convenient to create a function that encodes those biases and domains, so that you
# don't have to keep specifying them as parameters.  [This is not currying, strictly speaking.]
curryBiasDiffs <- function(bias1, bias2, doms) { function(df){biasDiffs(df, bias1, bias2, doms)} }

# applies a curryBiasDiffs function (bdfn), adding a model lable and a longer description, and by default setting rawsum=="raw"
# DOESNT WORK
#applyBiasDiffsWithId <- function(bdfn, df, model, desc){
#  newdf <- cbind(bdfn(df), model=model, desc=desc, rawsum="raw")
#  newdf <- rbind(newdf, data.frame(colMeans(newdf[,1:2]), model=model, desc=desc, rawsum="mean")) # NOT RIGHT
#  newdf
#}

#------------------------------------------------------

# This function can be used to create buckets into which activation avgs can be sorted e.g. for a histogram.
# Given a vector of evenly-spaced "foci"--points to which activation averages will usually 
# converge--return a vector of points shifted half the distance between two foci, with
# -1 and 1 added at the ends instead of whatever close point the shift would create.
# Assumes that spacing between focal values are the same, except possibly at endpoints.
# Second argument can be used to divide bucket size by that number, while preserving symmetry around foci.
foci2intervals <- function(foci, divs=1){
  dif <- (foci[3]-foci[2])/divs  # start from index 2, in case the first entry doesn't follow the pattern, e.g. it's a -1 that was added in.
  ints <- seq(foci[1]-dif/2, foci[length(foci)]+dif/2, dif)
  if (ints[1] > -1) {
    ints <- c(-1, ints)
  }
  if (ints[length(ints)] < 1) {
    ints <- c(ints, 1)
  }
  ints
}

foci2intervals.old <- function(foci, divs=1){
  dif <- foci[3]-foci[2]  # start from index 2, in case the first entry doesn't follow the pattern, e.g. it's a -1 that was added in.
  c(-1, foci[1]-dif/2, foci+dif/2, 1)
}


# notes toward ehanced version that will subdivide around the focal values:
# unlist(lapply(butlast(butfirst(cv.ints)), function(x){seq(x,x+0.1376923*4/5,len=5)}))

addFactors2df <- function(df, col, foci) {
  newcol <- paste0(col, ".fac")
  df[,newcol] <- cut(df[,col], foci2intervals(cv.foci))
  df
}

# given a dataframe of e.g. run means, with columns propn domain 1, propn domain 2, bias,
# produce a dataframe of frequencies indexed by propn domain cut intervals, and bias:
# NOTE: must have a column called "rawsum". Only rows with value "raw" will be processed.
DF2freqDF <- function(aDF, dom1, dom2, dom1intervals, dom2intervals, catvar="bias") {
  # add cut intervals to the internal copy of the input df:
  cookedDF <- DF2freqDF.prepare.aDF(aDF, dom1, dom2, dom1intervals, dom2intervals)
  cats <- levels(cookedDF[[catvar]])
  print(paste("cats:", cats)) # DEBUG
  # I don't know how to do this without a loop:
  freqDF <- NULL
  for (i in 1:length(cats)) {
    freqDF <- rbind(freqDF, DF2freqDF.component(cats[i], cookedDF, dom1, dom2))
  }
  print(names(freqDF)) # DEBUG
  names(freqDF) <- c(dom1, dom2, "Freq", catvar)
  freqDF
}

# DF2freqDF helper function
DF2freqDF.prepare.aDF <- function(aDF, dom1, dom2, dom1intervals, dom2intervals) {
  # add cut intervals to the internal copy of the input df:
  aDF <- aDF[aDF$rawsum=="raw",] # strip out means, etc.
  aDF$cut1 <- cut(aDF[[dom1]], dom1intervals)
  aDF$cut2 <- cut(aDF[[dom2]], dom2intervals)
  aDF
}

# DF2freqDF helper function
DF2freqDF.component <- function(bias, aDF, dom1, dom2) {
  biasrows <- aDF$bias==bias
  aDFbiasrows <- aDF[biasrows, , drop=F]
  dom1rows <- aDFbiasrows[["cut1"]]
  dom2rows <- aDFbiasrows[["cut2"]]
  freqDF <- as.data.frame(table(dom1rows, dom2rows))
  freqDF$bias <- bias
  freqDF
}

# OLD VERSION THAT ASSUMED CATEGORY VAR WAS CALLED "bias"
# given a dataframe of e.g. run means, with columns propn domain 1, propn domain 2, bias,
# produce a dataframe of frequencies indexed by propn domain cut intervals, and bias:
#DF2freqDF <- function(aDF, dom1, dom2, dom1intervals, dom2intervals) {
#  # add cut intervals to the internal copy of the input df:
#  cookedDF <- DF2freqDF.prepare.aDF(aDF, dom1, dom2, dom1intervals, dom2intervals)
#  #aDF <- aDF[aDF$rawsum=="raw",] # strip out means, etc.
#  #aDF$cut1 <- cut(aDF[[dom1]], dom1intervals)
#  #aDF$cut2 <- cut(aDF[[dom2]], dom2intervals)
#  biases <- levels(cookedDF$bias)
#  # I don't know how to do this without a loop:
#  freqDF <- NULL
#  for (i in 1:length(biases)) {
#    freqDF <- rbind(freqDF, DF2freqDF.component(biases[i], cookedDF, dom1, dom2))
#  }
#  names(freqDF) <- c(dom1, dom2, "Freq", "bias")
#  freqDF
#}

# utility to add top-level dimension names to a multi-RA if didn't do it already:
addTopDimNamesToMultiRA <- function(RA) {
  dimns <- dimnames(RA)
  persnames <- dimns[[1]]
  propnames <- dimns[[2]]
  ticknames <- dimns[[3]]
  runnames  <- dimns[[4]]
  dimnames(RA) <- list(person=persnames, proposition=propnames, tick=ticknames, run=runnames)
  RA
}

# return absolute difference between max and min values
# (useful for finding sets of persons who differ significantly on a particular proposition)
spread <- function(x){abs( max(x) - min(x) )}

# Given a domain-specific subset of a multirun array e.g. produced by multiRA2punditFreeDomRA(),
# i.e. with dimensions person, proposition, tick, run (even if there is only one tick),
# return a vector of names of runs which have at least proposition with disagreement between
# persons greater than tolerance (where disagreement is measured by spread(), i.e. by distance
# between max and min activations across persons for a proposition).
# ACTUALLY, there's no reason to restrict this to a domain.  However, you do want to
# remove pundits, since they are unlikely to agree with everyone, in general.
# NOTE tickIndex is relative to length of domMultiRA, if numeric.  e.g. if domMultiRA has
# only one tick, originally tick 1500, then tickIndex should be 1, or the string "1500".
findRunsWithDisagreement <- function(domMultiRA, tolerance, tickIndex=dim(domMultiRA)[3], verbose=F) {  # defaults to last tick
  spreads <- apply(domMultiRA[,,tickIndex, , drop=F], c(2,4), spread)  # return 2D array of spreads at tickIndex for each propn in each run
  spreadsGTtolerance <- spreads > tolerance  # change preceding array into propn X run array of T/F's, TRUE iff a given spread is > tolerance
  disagreeableRunPositions <- apply(spreadsGTtolerance, c(2), any) # return vector containing, for each run, TRUE iff some propn in run had spread > tolerance
  runNames <- dimnames(domMultiRA)[[4]]

  if (verbose){
    for (i in 1:length(disagreeableRunPositions)){
       if (disagreeableRunPositions[i]) {
         print(cbind(spreads,spreadsGTtolerance))
       }
    }
  }

  runNames[disagreeableRunPositions]   # return names of runs which had some propn with spread > tolerance
}

# side-effecting increment operators
inc <- function(i){ eval.parent(substitute(i <- i + 1)); i } # inc then return, like like ++i
postinc <- function(i){ old = i; eval.parent(substitute(i <- i + 1)); old} # increment and return old val, like i++

##############################################################
# file read functions

# Return list of all files *.csv in the current working directory.  (Use setwd() to change dir)
getcsvnames <- function() {list.files(pattern="*.csv")}

getcsvnamesInDir <- function(datadir) {
  currdir <- getwd()
  setwd(datadir)
  csvs <- getcsvnames()
  setwd(currdir)
  csvs
}

# Just a wrapper around read.csv to allow adding print statements, etc.
readcsv <- function(csv) {
  cat(csv, " ", sep="")
  read.csv(csv)
}

# Given a list or vector of filenames, return a list of dataframes, one for each input file.
readcsvs <- function(csvs) { dfs <- lapply(csvs, readcsv) ; cat("\n"); dfs}

# Given a list or vector of filenames, return a list of arrays created by df2RA(), one for each input file.
read2RAs <- function(csvs, firstTick=1) {
  dfs2RAs(readcsvs(csvs), firstTick=firstTick)
}

dfs2RAs <- function(dframes, firstTick=1) {
  lapply(dframes, df2RA, firstTick=firstTick)
}

dfs2multirunRA <- function(dframes, firstTick=1) {
  RAs <- dfs2RAs(dframes, firstTick=firstTick)
  print("done converting dataframes to arrays")
  RAs2multirunRA(RAs, stripcsv(getcsvnames()))
}

# read2multirunRA (v3)
# Given a list or vector of filenames, return a 4-dimensional array created by RAs2multirunRA()
# csvs: csv file names to process, assumed to have same persons, propositions, and number of pop-ticks.
# firstTick: starting tick to keep in data
# [This version (1/2013) uses less memory than previous versions by preallocating the full array
# to be returned, and then filling it with data one csv file at a time.  The original version
# was extremely simple--really just a wrapper--but tried to process all of the csvs at once
# before constructing the array, which was extremely memory intensive for more than a few csv files.  
# The version just before this one was able to process csvs individually, but then abind'ed them into 
# a new array one by one, which resulted in excessive memory use when the number of csvs was large.]
read2multirunRA <- function(csvs, firstTick=1) {
  n.runs <- length(csvs)

  # process the first csv file so that we know the dimensions of the per-run component mras
  mra.run1 <- RAs2multirunRA(read2RAs(csvs[1], firstTick=firstTick), stripcsv(csvs[1]))  # stripcsv(csvs[1]) would be a dimname, but will be lost and then restored
  mra.dims <- dim(mra.run1) # CHECK is last dim getting dropped or being retained? retained I think.
  mra.dims[4] <- n.runs

  # now construct an array as large as the final array we'll need.  then we'll fill it in.
  mra <- array(0, mra.dims) # SHOULD WE BE INITIALIZING THE FIRST PART WITH mra.run1 HERE--IS THAT FASTER?

  # fill the first run
  mra[,,,1] <- mra.run1

  # process and fill in the other runs:
  if (length(csvs) > 1) {
    for (i in 2:n.runs) {  # IS THIS RIGHT???
      mra[,,,i] <- RAs2multirunRA(read2RAs(csvs[i], firstTick=firstTick), stripcsv(csvs[i])) # see comment above re stripcsv(csvs[i])
    }
  }

  # now add the dim names:
  mra <- restoreTopDimnames(mra)
  dimnames(mra)[1:3] <- dimnames(mra.run1)[1:3]
  dimnames(mra)$run <- stripcsv(csvs)

  mra
}

# read2multirunRA OLD VERSION (v2)
# Given a list or vector of filenames, return a 4-dimensional array created by RAs2multirunRA()
# csvs: csv file names to process
# The optional parameters exist to avoid using gobs of memory, causing the machine to thrash:
# firstTick: starting tick to keep in data
# perload: number of csv files to process at one time
read2multirunRA.old <- function(csvs, firstTick=1, perload=length(csvs)) {
  require(abind)

  mra <- NULL

  for (i in seq(1, length(csvs), perload)) {
    runidxs <- i:(i+perload-1)  # -1 because we want the seq *up to* but not including next i
    cat("abind-ing run(s)", runidxs, "to main array ...\n")
    mra <- abind( mra, 
                 RAs2multirunRA(read2RAs(csvs[runidxs], firstTick=firstTick),
		                stripcsv(csvs[runidxs])) )
  }

  restoreTopDimnames(mra) # add back the top-level dim names that abind loses
}

# original memory hog version of read2multirunRA (v1):
#read2multirunRA <- function(csvs, firstTick=1) {
#  RAs2multirunRA(read2RAs(csvs, firstTick=firstTick), stripcsv(csvs))
#}

# Add back the top-level dim names that abind loses:
restoreTopDimnames <- function(mra) {
  dimnames(mra) <- list(person=dimnames(mra)[[1]], 
                        proposition=dimnames(mra)[[2]], 
                        tick=dimnames(mra)[[3]], 
                        run=dimnames(mra)[[4]])
  mra
}


# run read2multirunRA in specified directory, returning to current directory when done:
read2multirunRAfromDir <- function(datadir, firstTick=1) {
  currdir <- getwd()
  setwd(datadir)
  multiRA <- read2multirunRA(getcsvnames(), firstTick)
  setwd(currdir)
  multiRA
}

# run read2multirunRA in specified directory, returning to current directory when done:
read2dfsFromDir <- function(datadir) {
  currdir <- getwd()
  setwd(datadir)
  dfs <- readcsvs(getcsvnames())
  setwd(currdir)
  dfs
}

getNumPundits <- function(multiRA) {
  length(grep(paste0("^", punditPrefix), dimnames(multiRA)[[1]]))
}

# given a multi-run array and propn domain name string, strip pundits and return a list containing domain-specific array
multiRA2punditFreeDomRA <- function(multiRA, dom) {
  removePersons(multiRA2domRA(multiRA, dom), punditPrefix)
}

removePersons <- function(multiRA, personPrefix) {
  multiRA[grep(paste0("^", personPrefix), dimnames(multiRA)[[1]], invert=TRUE),,, , drop=FALSE]
}


domRA2personMeanMat <- function(domRA) { apply(domRA, c(1,3), mean) }

personMeanMat2runMeanVec <- function(pMeanMat) { apply(pMeanMat, c(2), mean) }

domRA2runMeanVec <- function(domRA) {
  personMeanMat2runMeanVec(domRA2personMeanMat(domRA))
}

# Produce a dataframe of mean activations in two domains
# Example of typical usage: 
#   mi <- multiRA2meanDF(mra2i, "H", "P", firstTick=1500)
# which would produce a two-column dataframe of per-run means for hunting and parenting
multiRA2meanDF <- function(multiRA, dom1, dom2, lastTick=dim(multiRA)[3], firstTick=lastTick) {
  mra <- stripRunPaths(multiRA)
  df <- data.frame(apply(multiRA2punditFreeDomRA(mra[,,firstTick:lastTick,,drop=F], dom1), 4, mean),
                   apply(multiRA2punditFreeDomRA(mra[,,firstTick:lastTick,,drop=F], dom2), 4, mean))
  names(df) <- c(dom1, dom2)
  df$rawsum <- "raw"
  df <- rbind(df, c(colMeans(df[,1:2]), rawsum="mean"))
  # One of the steps above--not sure which--coerces the numbers to strings; undo that change:
  df[[dom1]] <- as.numeric(df[[dom1]])
  df[[dom2]] <- as.numeric(df[[dom2]])
  df
}


# Given a list [i.e. with list(), not c()] of multi-run arrays, and a list or vector of strings
# to use as names of the bias of each array, and two prefix strings for propositions, calls
# multiRA2meanDF repeatedly on the arrays using the two prefix strings, and then combines the
# resulting dataframes into one dataframe by passing them along with the vector/list of bias strings 
# to combineMeanDFsWithBiases.
# Examples:
# df <- multiRAs2combinedMeanDF(list(mra1, mra2, mra3), c("virus", "beast", "both"), "CV", "CB")
# df <- multiRAs2combinedMeanDF(list(mra[2:11,,,], mra[12:21,,,]), c("beast", "virus"), "CV", "CB")
multiRAs2combinedMeanDF <- function(mras, biases, dom1, dom2, lastTick=dim(mras[[1]])[3], firstTick=lastTick) {
  combineMeanDFsWithBiases(lapply(mras, multiRA2meanDF, dom1, dom2, lastTick, firstTick), biases)
}

# dfs must be a list, but biases can be either a list or a vector
combineMeanDFsWithBiases <- function(dfs=NULL, biases=NULL) {
  if (length(dfs) == 0 || length(biases) == 0) {stop("dfs or biases is empty.")}
  if (length(dfs) != length(biases)) {stop("lengths of dfs and biases are not the same.")}

  df <- NULL

  for (i in 1:length(dfs)){
    dfs[[i]]$bias <- biases[[i]] # double brackets works with both lists and vectors; single brackets only for vectors here
  }

  do.call(rbind, dfs)
}

#combineMeanDFsWithBiases.old <- function(df1, bias1, df2, bias2) {
#  # note these are changes to the internal copies, not the function arguments:
#  df1$bias <- bias1
#  df2$bias <- bias2
#  rbind(df1, df2)
#}

##############################################################
# NOT CURRENTLY USED:
# these definitions must be coordinated with each other and with POPCO
# stripMetaCols <- function(dframe) {dframe[3:length(dframe)]} # return dataframe without the meta columns
# metaColnames <- c("RUNID", "TICK") # column names that don't rep personal propns
# metaColnamesRegexp <- paste0("^(", paste(metaColnames, collapse="|"), ")$") # regexp that will find the meta col names

##############################################################
# functions for extracting meaningful labels

# these extract various parts of proposition names
componentBeforeFirstUnderscore <- function(propnms) {unique(sub("_.*", "", propnms))} # extract unique person names from personal propn names
persPropNames2genPropNames <- function(propnms) {unique(sub("[^_]*_", "", propnms))}  # extract unique generic propn names from personal propn names
persPropNames2persNames <- function(propnms) {componentBeforeFirstUnderscore(propnms)} # extract unique person names from personal propn names
genPropNames2domNames <- function(propnms) {componentBeforeFirstUnderscore(propnms)} # extract unique domain names from generic propn names

countPropsInDomain <- function(dom, propnms=c()) {length(grep(paste0("^", dom, "_"), propnms))}

# OBSOLETE - written for pre-Nov2012 csv formatting, in which R replaced dashes with dots, rather than Lisp replace dashes with underscores:
#genPropNames2domNames  <- function(propnms) {unique(sub("([^.]*)\\..*", "\\1", propnms))} # extract domain names (propn prefixes) from generic propn names
#persPropNames2domNames <- function(propnms) {genPropNames2domNames(persPropNames2genPropNames(propnms))} # extract domain names (propn prefixes) from personal propn names

# NOTE the following abstractions of the preceding definitions are not currently needed, but were, and maybe will be.
# these can be used to extract the same information if the meta colnames are mixed in with the propn names
# allColNames2propNames <- function(colnms) {grep(metaColnamesRegexp, colnms, value=TRUE, invert=TRUE)} # strip non-proposition metadata column names NOT CURRENTLY USED
#allColNames2propNames <- function(colnms) {colnms} # no-op; might be replaced later with something like the preceding
#allColNames2gPropNames <- function(colnms) {persPropNames2genPropNames(allColNames2propNames(colnms))}  # extract unique generic propn names from column names
#allColNames2persNames <- function(colnms) {persPropNames2persNames(allColNames2propNames(colnms))} # extract unique person names from column names
#allColNames2domNames  <- function(colnms) {persPropNames2domNames(allColNames2propNames(colnms))} # extract domain names (propn prefixes) from column names

stripcsv <- function(filenames) {gsub("\\.csv$", "", filenames)} # given vec or list of filenames, return vec of strings with ".csv" removed from end

##############################################################
# df2RA
# Function to create array with dims: person, propn, tick 
# from popco dataframe created by read.csv.
# i.e. for each tick, there's a personXpropn matrix.
# [When printed in R, what you'll see is a series of tick-indexed matrices
# going down the page, each matrix having person rows and propn cols.]
# Arguments:
# dframe: a data frame to use as input data
# firstTick: if passed, the first tick of the dataframe to include (defaults to 1)
# lastTick: if passed, the last tick of the dataframe to include (defaults to the number of rows in the dataframe)
# i.e. the returned RA will go from firstTick to lastTick, inclusive.

df2RA <- function(dframe, firstTick=1, lastTick=nrow(dframe)) {
  cat("converting dframe to array, using ticks", firstTick, "to", lastTick, "\n")
  # extract desired dimensions and labels from the dataframe:
  cols = colnames(dframe)
  persnames = persPropNames2persNames(cols)     # ; print(persnames)
  propnames = persPropNames2genPropNames(cols)  # ; print(propnames)
  ticks = firstTick:lastTick
  nticks = length(ticks)
  npersons = length(persnames)
  npropns = length(propnames)

  # Create version of the array we want, but with the inner matrices
  # defined by first two dimensions flipped along the diagonal.  
  # Seems to be the only easy way to do it.
  # See notes below for further explanation.
  flippedmats = array( t(dframe[firstTick:lastTick,]) ,  c(npropns,   npersons,  nticks) , 
                       dimnames=list(propositions=propnames, persons=persnames, ticks=ticks) )

  # Now return version with inner (i.e. first two) dimensions swapped,
  # so that the resulting array is what we want:
  aperm(flippedmats, c(2, 1, 3) )
}

##############################################################
# RAs2multirunRA
# Given a list of 3-D arrays, each of the kind produced by df2RA, and a list
# of run names of the runs which generated the data for those arrays,
# return a 4-D array in which each element along the 4th dimension is
# one of the original 3-D arrays, in order.
# Note: It appears that the arguments can be vectors or lists.

RAs2multirunRA <- function(RAs, runIDs) {
  cat("converting arrays to mulitrun array\n")
  numRAs = length(RAs)

  # error checking
  if (numRAs != length(runIDs)) { stop("RAs and runIDs have different lengths.") }
  # ideally we could also check that dimensions and dimnames are all same

  RAsInOneVec <- mapply(c, unlist(RAs)) # squash all data from arrays into a single vector

  newDims <- c(dim(RAs[[1]]), numRAs) # construct dimensions of output array - assumes all arrays same
  newDimnames <- dimnames(RAs[[1]]) # again, assuming all arrays are same
  newDimnames[[4]] <- runIDs # extend list of lists of dimnames to include the run ids as names along 4th dimension

  # construct a 4-D array containing each member of RAs as one element along 4th dim:
  newRA <- array(RAsInOneVec, newDims, newDimnames)
  addTopDimNamesToMultiRA(newRA) # returns a modified array
}

##############################################################
# RA2domRA
# Extract an array corresponding to a domain of belief from
# an array containing all beliefs.
# The second argument, dom, is a string for a POPCO proposition 
# prefix representing a domain of belief.

# for single run array
RA2domRA <- function(ra, dom) {
  ra[ , getDomainColnums(ra, dom) , ]  # return an array with only columns we want
}

# for multi-run array
multiRA2domRA <- function(ra, dom) {
  ra[ , getDomainColnums(ra, dom) , , , drop=FALSE]  # return an array with only columns we want, keeping all dims even if length=1
}

# Given an array and a domain string, returns the indexes
# of colums withpropositions from that domain.
getDomainColnums <- function(ra, dom) {
  propnames = dimnames(ra)[2][[1]] # get the proposition names from the ra
  regx = paste0("^", dom, ".")     # we'll search for this string
  grep(regx, propnames)  # get indexes of columns we want
}

##############################################################
# Some notes on how strippedDf2RA works: 
# It seems odd to have to flip the dframe using t, and then flip the inner
# matrices later using aperm, but that's the only way I've figured out
# to do it.  It obviously could be done in one step, but it's easier to
# read in two steps.  Actually, the whole thing could be done in one line.
# The reason for the transform and then permutation is that array() insists
# on reading in colum,row order, but I want my data read in row,column order.
# So I accomodate array()'s wishes to get the data into an array, and then undo 
# the dimensional damage after the fact.
##############################################################
