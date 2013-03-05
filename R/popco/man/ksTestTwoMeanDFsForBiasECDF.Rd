\name{ksTestTwoMeanDFsForBiasECDF}
\alias{ksTestTwoMeanDFsForBiasECDF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ksTestTwoMeanDFsForBiasECDF
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ksTestTwoMeanDFsForBiasECDF(df1, df2, dom, bias, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df1}{
%%     ~~Describe \code{df1} here~~
}
  \item{df2}{
%%     ~~Describe \code{df2} here~~
}
  \item{dom}{
%%     ~~Describe \code{dom} here~~
}
  \item{bias}{
%%     ~~Describe \code{bias} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (df1, df2, dom, bias, ...) 
{
    ksTestTwoMeanDFsECDF(df1[df1$bias == bias, ], df2[df2$bias == 
        bias, ], dom, ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
