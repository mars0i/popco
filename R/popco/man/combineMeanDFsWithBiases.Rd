\name{combineMeanDFsWithBiases}
\alias{combineMeanDFsWithBiases}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{combineMeanDFsWithBiases
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
combineMeanDFsWithBiases(dfs = NULL, biases = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfs}{
%%     ~~Describe \code{dfs} here~~
}
  \item{biases}{
%%     ~~Describe \code{biases} here~~
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
function (dfs = NULL, biases = NULL) 
{
    if (length(dfs) == 0 || length(biases) == 0) {
        stop("dfs or biases is empty.")
    }
    if (length(dfs) != length(biases)) {
        stop("lengths of dfs and biases are not the same.")
    }
    df <- NULL
    for (i in 1:length(dfs)) {
        dfs[[i]]$bias <- biases[[i]]
    }
    do.call(rbind, dfs)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
