addTopDimNamesToMultiRA <-
function(RA) {
  dimns <- dimnames(RA)
  persnames <- dimns[[1]]
  propnames <- dimns[[2]]
  ticknames <- dimns[[3]]
  runnames  <- dimns[[4]]
  dimnames(RA) <- list(person=persnames, proposition=propnames, tick=ticknames, run=runnames)
  RA
}
