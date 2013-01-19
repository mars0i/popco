\name{findRunsWithDisagreement}
\alias{findRunsWithDisagreement}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{findRunsWithDisagreement
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
findRunsWithDisagreement(domMultiRA, tolerance, tickIndex = dim(domMultiRA)[3])
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{domMultiRA}{
%%     ~~Describe \code{domMultiRA} here~~
}
  \item{tolerance}{
%%     ~~Describe \code{tolerance} here~~
}
  \item{tickIndex}{
%%     ~~Describe \code{tickIndex} here~~
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
function (domMultiRA, tolerance, tickIndex = dim(domMultiRA)[3]) 
{
    spreads <- apply(domMultiRA[, , tickIndex, , drop = F], c(2, 
        4), spread)
    spreadsGTtolerance <- spreads > tolerance
    disagreeableRunPositions <- apply(spreadsGTtolerance, c(2), 
        any)
    runNames <- dimnames(domMultiRA)[[4]]
    runNames[disagreeableRunPositions]
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
