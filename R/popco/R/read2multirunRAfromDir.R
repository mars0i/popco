read2multirunRAfromDir <-
function(datadir, firstTick=1) {
  currdir <- getwd()
  setwd(datadir)
  multiRA <- read2multirunRA(getcsvnames(), firstTick)
  setwd(currdir)
  multiRA
}
