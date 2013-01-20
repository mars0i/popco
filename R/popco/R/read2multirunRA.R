read2multirunRA <-
function(csvs, firstTick=1) {
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
