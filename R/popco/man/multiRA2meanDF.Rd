\name{multiRA2meanDF}
\alias{multiRA2meanDF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{multiRA2meanDF
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
multiRA2meanDF(multiRA, dom1, dom2, lastTick = dim(multiRA)[3], firstTick = lastTick)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{multiRA}{
%%     ~~Describe \code{multiRA} here~~
}
  \item{dom1}{
%%     ~~Describe \code{dom1} here~~
}
  \item{dom2}{
%%     ~~Describe \code{dom2} here~~
}
  \item{lastTick}{
%%     ~~Describe \code{lastTick} here~~
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
function (multiRA, dom1, dom2, lastTick = dim(multiRA)[3], firstTick = lastTick) 
{
    mra <- stripRunPaths(multiRA)
    df <- data.frame(apply(multiRA2punditFreeDomRA(mra[, , firstTick:lastTick, 
        , drop = F], dom1), 4, mean), apply(multiRA2punditFreeDomRA(mra[, 
        , firstTick:lastTick, , drop = F], dom2), 4, mean))
    names(df) <- c(dom1, dom2)
    df$rawsum <- "raw"
    df <- rbind(df, c(colMeans(df[, 1:2]), rawsum = "mean"))
    df[[dom1]] <- as.numeric(df[[dom1]])
    df[[dom2]] <- as.numeric(df[[dom2]])
    df
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
