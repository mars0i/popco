DF2freqDF <-
function(aDF, dom1, dom2, dom1intervals, dom2intervals, catvar="bias") {
  # add cut intervals to the internal copy of the input df:
  cookedDF <- DF2freqDF.prepare.aDF(aDF, dom1, dom2, dom1intervals, dom2intervals)
  cats <- levels(cookedDF[[catvar]])
  print(paste("cats:", cats)) # DEBUG
  # I don't know how to do this without a loop:
  freqDF <- NULL
  for (i in 1:length(cats)) {
    freqDF <- rbind(freqDF, DF2freqDF.component(cats[i], cookedDF, dom1, dom2))
  }
  print(names(freqDF)) # DEBUG
  names(freqDF) <- c(dom1, dom2, "Freq", catvar)
  freqDF
}
