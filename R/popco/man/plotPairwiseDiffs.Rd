\name{plotPairwiseDiffs}
\alias{plotPairwiseDiffs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plotPairwiseDiffs
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotPairwiseDiffs(mra, ymax = 2, ymin = -2, type = "l", pch = ".", jitter = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mra}{
%%     ~~Describe \code{mra} here~~
}
  \item{ymax}{
%%     ~~Describe \code{ymax} here~~
}
  \item{ymin}{
%%     ~~Describe \code{ymin} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{pch}{
%%     ~~Describe \code{pch} here~~
}
  \item{jitter}{
%%     ~~Describe \code{jitter} here~~
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
function (mra, ymax = 2, ymin = -2, type = "l", pch = ".", jitter = 0) 
{
    plotcolors <- c("black", "blue", "red", "purple", "green", 
        "darkorange", "darkgreen", "navyblue")
    colorsep <- "   "
    colorkey <- ""
    numruns <- dim(mra)[4]
    numticks <- dim(mra)[3]
    plot(0, type = "l", xlim = c(1, numticks), ylim = c(ymin, 
        ymax), xlab = paste(sub(".*/", "", dimnames(mra)$run), 
        collapse = ", "), main = "max and min differences between runs")
    abline(h = 0, col = "gray", lty = 3)
    runpairs <- expand.grid(a = 1:numruns, b = 1:numruns)
    runpairs <- runpairs[runpairs$a > runpairs$b, ]
    for (i in 1:dim(runpairs)[1]) {
        runA <- runpairs[i, 1]
        runB <- runpairs[i, 2]
        diffs1 <- mra[, , , runA] - mra[, , , runB]
        diffs2 <- mra[, , , runB] - mra[, , , runA]
        if (max(diffs1) > max(diffs2)) {
            diffs <- diffs1
            run1 <- runA
            run2 <- runB
        }
        else {
            diffs <- diffs2
            run1 <- runB
            run2 <- runA
        }
        if (jitter != 0) {
            diffs <- diffs + rnorm(1, sd = jitter)
        }
        colorkey <- paste0(colorkey, plotcolors[i], ": ", run1, 
            "-", run2, colorsep)
        lines(apply(diffs, 3, max), type = type, pch = pch, col = plotcolors[i])
        lines(apply(diffs, 3, min), type = type, pch = pch, col = plotcolors[i])
    }
    colorkey <- substr(colorkey, 1, nchar(colorkey) - nchar(colorsep))
    mtext(colorkey, 1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
