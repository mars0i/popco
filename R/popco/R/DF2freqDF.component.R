DF2freqDF.component <-
function(bias, aDF, dom1, dom2) {
  biasrows <- aDF$bias==bias
  aDFbiasrows <- aDF[biasrows, , drop=F]
  dom1rows <- aDFbiasrows[["cut1"]]
  dom2rows <- aDFbiasrows[["cut2"]]
  freqDF <- as.data.frame(table(dom1rows, dom2rows))
  freqDF$bias <- bias
  freqDF
}
