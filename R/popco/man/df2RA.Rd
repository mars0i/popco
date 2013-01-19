\name{df2RA}
\alias{df2RA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{df2RA
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
df2RA(dframe, firstTick = 1, lastTick = nrow(dframe))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dframe}{
%%     ~~Describe \code{dframe} here~~
}
  \item{firstTick}{
%%     ~~Describe \code{firstTick} here~~
}
  \item{lastTick}{
%%     ~~Describe \code{lastTick} here~~
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
function (dframe, firstTick = 1, lastTick = nrow(dframe)) 
{
    cat("converting dframe to array, using ticks", firstTick, 
        "to", lastTick, "\n")
    cols = colnames(dframe)
    persnames = persPropNames2persNames(cols)
    propnames = persPropNames2genPropNames(cols)
    ticks = firstTick:lastTick
    nticks = length(ticks)
    npersons = length(persnames)
    npropns = length(propnames)
    flippedmats = array(t(dframe[firstTick:lastTick, ]), c(npropns, 
        npersons, nticks), dimnames = list(propositions = propnames, 
        persons = persnames, ticks = ticks))
    aperm(flippedmats, c(2, 1, 3))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
