\name{DF2freqDF}
\alias{DF2freqDF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{DF2freqDF
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DF2freqDF(aDF, dom1, dom2, dom1intervals, dom2intervals, catvar = "bias")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{aDF}{
%%     ~~Describe \code{aDF} here~~
}
  \item{dom1}{
%%     ~~Describe \code{dom1} here~~
}
  \item{dom2}{
%%     ~~Describe \code{dom2} here~~
}
  \item{dom1intervals}{
%%     ~~Describe \code{dom1intervals} here~~
}
  \item{dom2intervals}{
%%     ~~Describe \code{dom2intervals} here~~
}
  \item{catvar}{
%%     ~~Describe \code{catvar} here~~
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
function (aDF, dom1, dom2, dom1intervals, dom2intervals, catvar = "bias") 
{
    cookedDF <- DF2freqDF.prepare.aDF(aDF, dom1, dom2, dom1intervals, 
        dom2intervals)
    cats <- levels(cookedDF[[catvar]])
    print(paste("cats:", cats))
    freqDF <- NULL
    for (i in 1:length(cats)) {
        freqDF <- rbind(freqDF, DF2freqDF.component(cats[i], 
            cookedDF, dom1, dom2))
    }
    print(names(freqDF))
    names(freqDF) <- c(dom1, dom2, "Freq", catvar)
    freqDF
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
