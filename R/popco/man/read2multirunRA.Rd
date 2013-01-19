\name{read2multirunRA}
\alias{read2multirunRA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{read2multirunRA
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read2multirunRA(csvs, firstTick = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{csvs}{
%%     ~~Describe \code{csvs} here~~
}
  \item{firstTick}{
%%     ~~Describe \code{firstTick} here~~
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
function (csvs, firstTick = 1) 
{
    n.runs <- length(csvs)
    mra.run1 <- RAs2multirunRA(read2RAs(csvs[1], firstTick = firstTick), 
        stripcsv(csvs[1]))
    mra.dims <- dim(mra.run1)
    mra.dims[4] <- n.runs
    mra <- array(0, mra.dims)
    mra[, , , 1] <- mra.run1
    if (length(csvs) > 1) {
        for (i in 2:n.runs) {
            mra[, , , i] <- RAs2multirunRA(read2RAs(csvs[i], 
                firstTick = firstTick), stripcsv(csvs[i]))
        }
    }
    mra <- restoreTopDimnames(mra)
    dimnames(mra)[1:3] <- dimnames(mra.run1)[1:3]
    dimnames(mra)$run <- stripcsv(csvs)
    mra
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
