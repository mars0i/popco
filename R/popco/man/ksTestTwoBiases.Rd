\name{ksTestTwoBiases}
\alias{ksTestTwoBiases}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ksTestTwoBiases
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ksTestTwoBiases(df, dom, bias1, bias2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{dom}{
%%     ~~Describe \code{dom} here~~
}
  \item{bias1}{
%%     ~~Describe \code{bias1} here~~
}
  \item{bias2}{
%%     ~~Describe \code{bias2} here~~
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
function (df, dom, bias1, bias2, ...) 
{
    ksTestTwoMeanDFs(df[df$bias == bias1, ], df[df$bias == bias2, 
        ], dom, ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
