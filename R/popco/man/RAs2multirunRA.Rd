\name{RAs2multirunRA}
\alias{RAs2multirunRA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RAs2multirunRA
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RAs2multirunRA(RAs, runIDs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{RAs}{
%%     ~~Describe \code{RAs} here~~
}
  \item{runIDs}{
%%     ~~Describe \code{runIDs} here~~
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
function (RAs, runIDs) 
{
    cat("converting arrays to mulitrun array\n")
    numRAs = length(RAs)
    if (numRAs != length(runIDs)) {
        stop("RAs and runIDs have different lengths.")
    }
    RAsInOneVec <- mapply(c, unlist(RAs))
    newDims <- c(dim(RAs[[1]]), numRAs)
    newDimnames <- dimnames(RAs[[1]])
    newDimnames[[4]] <- runIDs
    newRA <- array(RAsInOneVec, newDims, newDimnames)
    addTopDimNamesToMultiRA(newRA)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
