ksTestTwoMeanDFsECDF <-
function(df1, df2, dom, ...) {
  vec1 <- df1[df1$rawsum=="raw", dom]
  vec2 <- df2[df2$rawsum=="raw", dom]
  ks.test(vec1, ecdf(vec2), ...)
}
