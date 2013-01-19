combineMeanDFsWithBiases.old <-
function(df1, bias1, df2, bias2) {
  # note these are changes to the internal copies, not the function arguments:
  df1$bias <- bias1
  df2$bias <- bias2
  rbind(df1, df2)
}
