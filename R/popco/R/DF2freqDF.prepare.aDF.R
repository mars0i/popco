DF2freqDF.prepare.aDF <-
function(aDF, dom1, dom2, dom1intervals, dom2intervals) {
  # add cut intervals to the internal copy of the input df:
  aDF <- aDF[aDF$rawsum=="raw",] # strip out means, etc.
  aDF$cut1 <- cut(aDF[[dom1]], dom1intervals)
  aDF$cut2 <- cut(aDF[[dom2]], dom2intervals)
  aDF
}
