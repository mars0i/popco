getcsvnamesInDir <-
function(datadir) {
  currdir <- getwd()
  setwd(datadir)
  csvs <- getcsvnames()
  setwd(currdir)
  csvs
}
