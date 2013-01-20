\name{DF2freqDF.component}
\alias{DF2freqDF.component}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{DF2freqDF.component
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DF2freqDF.component(bias, aDF, dom1, dom2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bias}{
%%     ~~Describe \code{bias} here~~
}
  \item{aDF}{
%%     ~~Describe \code{aDF} here~~
}
  \item{dom1}{
%%     ~~Describe \code{dom1} here~~
}
  \item{dom2}{
%%     ~~Describe \code{dom2} here~~
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
function (bias, aDF, dom1, dom2) 
{
    biasrows <- aDF$bias == bias
    aDFbiasrows <- aDF[biasrows, , drop = F]
    dom1rows <- aDFbiasrows[["cut1"]]
    dom2rows <- aDFbiasrows[["cut2"]]
    freqDF <- as.data.frame(table(dom1rows, dom2rows))
    freqDF$bias <- bias
    freqDF
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
