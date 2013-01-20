\name{read2multirunRA.old}
\alias{read2multirunRA.old}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{read2multirunRA.old
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read2multirunRA.old(csvs, firstTick = 1, perload = length(csvs))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{csvs}{
%%     ~~Describe \code{csvs} here~~
}
  \item{firstTick}{
%%     ~~Describe \code{firstTick} here~~
}
  \item{perload}{
%%     ~~Describe \code{perload} here~~
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
function (csvs, firstTick = 1, perload = length(csvs)) 
{
    require(abind)
    mra <- NULL
    for (i in seq(1, length(csvs), perload)) {
        runidxs <- i:(i + perload - 1)
        cat("abind-ing run(s)", runidxs, "to main array ...\n")
        mra <- abind(mra, RAs2multirunRA(read2RAs(csvs[runidxs], 
            firstTick = firstTick), stripcsv(csvs[runidxs])))
    }
    restoreTopDimnames(mra)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
