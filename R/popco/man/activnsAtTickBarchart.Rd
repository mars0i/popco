\name{activnsAtTickBarchart}
\alias{activnsAtTickBarchart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{activnsAtTickBarchart
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
activnsAtTickBarchart(mra, tick, run = 1, main = paste("tick", tick))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mra}{
%%     ~~Describe \code{mra} here~~
}
  \item{tick}{
%%     ~~Describe \code{tick} here~~
}
  \item{run}{
%%     ~~Describe \code{run} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
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
function (mra, tick, run = 1, main = paste("tick", tick)) 
{
    require(lattice)
    divadj <- 0.5
    propnms <- dimnames(mra)$proposition
    domnms <- genPropNames2domNames(propnms)
    domsizes <- unlist(lapply(domnms, countPropsInDomain, propnms = propnms))
    domdivs <- cumsum(domsizes[-length(domsizes)]) + divadj
    domcols <- c(rep("blue", domsizes[1]), rep("darkgreen", domsizes[2]), 
        rep("red", domsizes[3]), rep("darkorange", domsizes[4]))
    barchart(t(mra[, , tick, run]), groups = person, xlim = c(-1, 
        1), scales = list(cex = 0.5, y = list(alternating = 3)), 
        xlab = "activation", layout = c(4, 1), main = main, panel = function(y, 
            ...) {
            panel.abline(v = c(-0.5, 0.5), lty = 3, col = "gray")
            panel.abline(h = domdivs, lty = 2, col = "gray")
            panel.barchart(y = y, col = domcols[y], border = "transparent", 
                ...)
        })
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
