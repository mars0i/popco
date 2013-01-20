dfs2multirunRA <-
function(dframes, firstTick=1) {
  RAs <- dfs2RAs(dframes, firstTick=firstTick)
  print("done converting dataframes to arrays")
  RAs2multirunRA(RAs, stripcsv(getcsvnames()))
}
