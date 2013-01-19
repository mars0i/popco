findRunsWithDisagreement <-
function(domMultiRA, tolerance, tickIndex=dim(domMultiRA)[3]) {  # defaults to last tick
  spreads <- apply(domMultiRA[,,tickIndex, , drop=F], c(2,4), spread)  # return 2D array of spreads at tickIndex for each propn in each run
  spreadsGTtolerance <- spreads > tolerance  # change preceding array into propn X run array of T/F's, TRUE iff a given spread is > tolerance
  disagreeableRunPositions <- apply(spreadsGTtolerance, c(2), any) # return vector containing, for each run, TRUE iff some propn in run had spread > tolerance
  runNames <- dimnames(domMultiRA)[[4]]
  runNames[disagreeableRunPositions]   # return names of runs which had some propn with spread > tolerance
}
