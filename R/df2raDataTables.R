# ATTEMPT TO USE data.table

# Turn a (probably subsetted) multiple-run RA into a dataframe suitable
# for plotting with xyplot.  Note that this can quickly generate *a lot*
# of rows.
# [possibly rewrite to speed up using data.tables: # http://stackoverflow.com/questions/11486369/growing-a-data-frame-in-a-memory-efficient-manner/11486400#11486400]
# If interval > 1, then sample data every interval ticks to generate the dataframe.
multiRA2PersonPropnDT <- function(mra, interval=1) {
  require(data.table)

  dnames <- dimnames(mra)
  persons <- dnames[[1]]
  propns <- dnames[[2]]
  ticks <- dnames[[3]]
  runs <- dnames[[4]]

  domsInPropnOrder<- sub("_.*", "", propns) # list of domain names in the same order as propns they came from

  npersons <- length(persons)
  npropns <- length(propns)
  nticks <- length(ticks)
  nruns <- length(runs)

  #cat(npersons, npropns, nticks, runs)
  dummynums <- rep(NA_real_, nticks)
  dummystrings <- rep(NA_character_, nticks)

  # preallocate. sorta.
  newdt <- data.table(activn=dummynums,
                      person=dummystrings,
                      dom=dummystrings,
                      propn=dummystrings,
                      run=dummystrings,
                      tick=dummynums,
		      stringsAsFactors=FALSE) # otherwise R complains we add new strings

  cat("Making dataframe with", (npersons * npropns * nticks * nruns) / interval, "rows ...\n")

  # The "L"s force numbers to be ints rather than floats, which makes data.table happy.
  i <- 0L
  for (rn in 1:nruns) {
    for (pr in 1:npersons) {
      for (ppn in 1:npropns) {
        for (tck in seq(1,nticks,interval)) {
	  i <- i + 1L # row index

          # old dataframe version: newdt[i,] <- c(mra[pr,ppn,tck,rn], persons[pr], domsInPropnOrder[ppn], propns[ppn], runs[rn], tck)

          # seems fragile.  errors, segfaults ...
          newdt[i, activn := mra[pr,ppn,tck,rn] ]
	  newdt[i, person := persons[pr] ]
	  newdt[i, dom := domsInPropnOrder[ppn] ]
	  newdt[i, propn := propns[ppn] ]
	  newdt[i, run := runs[rn] ]
	  newdt[i, tick := tck ]

	  # Using set() is less clear but is supposed to be a lot faster than := (which is a lot faster than <-).
	  #set(newdt, i, 1L, mra[pr,ppn,tck,rn])
	  #set(newdt, i, 2L, persons[pr])
	  #set(newdt, i, 3L, domsInPropnOrder[ppn])
	  #set(newdt, i, 4L, propns[ppn])
	  #set(newdt, i, 5L, runs[rn])
	  #set(newdt, i, 6L, tck)

	  if (i %% 1000 == 0) {cat(i, "")} # don't let user get lonely, but don't be obnoxious
        }
      }
    }
  }

  newdt
}
