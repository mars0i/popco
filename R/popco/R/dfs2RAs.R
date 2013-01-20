dfs2RAs <-
function(dframes, firstTick=1) {
  lapply(dframes, df2RA, firstTick=firstTick)
}
