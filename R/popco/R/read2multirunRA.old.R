read2multirunRA.old <-
function(csvs, firstTick=1, perload=length(csvs)) {
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
