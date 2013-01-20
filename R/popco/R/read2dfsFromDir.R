read2dfsFromDir <-
function(datadir) {
  currdir <- getwd()
  setwd(datadir)
  dfs <- readcsvs(getcsvnames())
  setwd(currdir)
  dfs
}
