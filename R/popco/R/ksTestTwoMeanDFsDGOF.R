ksTestTwoMeanDFsDGOF <-
function(df1, df2, dom, ...) {
  require(dgof)
  vec1 <- df1[df1$rawsum=="raw", dom]
  vec2 <- df2[df2$rawsum=="raw", dom]
  dgof::ks.test(vec1, ecdf(vec2), ...)
}
