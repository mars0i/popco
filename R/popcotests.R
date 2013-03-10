# various tests useful for popco

# see also:
# findRunsWithDisagreement in df2ra.R

# Given two mean dfs produced by multiRA2meanDF, do a Kolmogorov-Smirnov 
# test with bootstrapped significance for sameness of distributions 
# for a given domain.  Uses Matching::ks.boot.
# ... can contain additional arguments to be passed to ks.boot.
ksBootTwoMeanDFs <- function(df1, df2, dom, ...) {
  require(Matching)
  vec1 <- df1[df1$rawsum=="raw", dom]
  vec2 <- df2[df2$rawsum=="raw", dom]
  ks.boot(vec1, vec2, ...)
}

# Same thing, but selects only rows with a particular bias from
# a dataframe which is a combination of different mean dfs.
ksBootTwoMeanDFsForBias <- function(df1, df2, dom, bias, ...) {
  ksBootTwoMeanDFs(df1[df1$bias==bias,], df2[df2$bias==bias,], dom, ...)
}

# given a dataframe which is a combination of mean dfs for different biases,
# perhaps produced by combineMeanDFsWithBiases, do a Kolmogorov-Smirnov 
# test between runs with two different biases in that dataframe
ksBootTwoBiases <- function(df, dom, bias1, bias2, ...) {
  ksBootTwoMeanDFs(df[df$bias==bias1,], df[df$bias==bias2,], dom, ...)
}

# given two mean dfs produced by multiRA2meanDF, do a Kolmogorov-Smirnov 
# test for sameness of distributions for a given domain.
# ... can contain additional arguments to be passed to ks.test.
ksTestTwoMeanDFs <- function(df1, df2, dom, ...) {
  vec1 <- df1[df1$rawsum=="raw", dom]
  vec2 <- df2[df2$rawsum=="raw", dom]
  ks.test(vec1, vec2, ...)
}

# Same thing, but selects only rows with a particular bias from
# a dataframe which is a combination of different mean dfs.
ksTestTwoMeanDFsForBias <- function(df1, df2, dom, bias, ...) {
  ksTestTwoMeanDFs(df1[df1$bias==bias,], df2[df2$bias==bias,], dom, ...)
}

# given a dataframe which is a combination of mean dfs for different biases,
# perhaps produced by combineMeanDFsWithBiases, do a Kolmogorov-Smirnov 
# test between runs with two different biases in that dataframe
ksTestTwoBiases <- function(df, dom, bias1, bias2, ...) {
  ksTestTwoMeanDFs(df[df$bias==bias1,], df[df$bias==bias2,], dom, ...)
}


# EXPERIMENTS:

ksTestTwoMeanDFsECDF <- function(df1, df2, dom, ...) {
  vec1 <- df1[df1$rawsum=="raw", dom]
  vec2 <- df2[df2$rawsum=="raw", dom]
  ks.test(vec1, ecdf(vec2), ...)
}

ksTestTwoMeanDFsForBiasECDF <- function(df1, df2, dom, bias, ...) {
  ksTestTwoMeanDFsECDF(df1[df1$bias==bias,], df2[df2$bias==bias,], dom, ...)
}

ksTestTwoMeanDFsDGOF <- function(df1, df2, dom, ...) {
  require(dgof)
  vec1 <- df1[df1$rawsum=="raw", dom]
  vec2 <- df2[df2$rawsum=="raw", dom]
  dgof::ks.test(vec1, ecdf(vec2), ...)
}

ksTestTwoMeanDFsForBiasDGOF <- function(df1, df2, dom, bias, ...) {
  ksTestTwoMeanDFsDGOF(df1[df1$bias==bias,], df2[df2$bias==bias,], dom, ...)
}
