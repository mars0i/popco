ksTestTwoBiases <-
function(df, dom, bias1, bias2, ...) {
  ksTestTwoMeanDFs(df[df$bias==bias1,], df[df$bias==bias2,], dom, ...)
}
