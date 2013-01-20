ksTestTwoMeanDFsForBias <-
function(df1, df2, dom, bias, ...) {
  ksTestTwoMeanDFs(df1[df1$bias==bias,], df2[df2$bias==bias,], dom, ...)
}
