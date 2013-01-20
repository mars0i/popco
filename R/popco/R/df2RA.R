df2RA <-
function(dframe, firstTick=1, lastTick=nrow(dframe)) {
  cat("converting dframe to array, using ticks", firstTick, "to", lastTick, "\n")
  # extract desired dimensions and labels from the dataframe:
  cols = colnames(dframe)
  persnames = persPropNames2persNames(cols)     # ; print(persnames)
  propnames = persPropNames2genPropNames(cols)  # ; print(propnames)
  ticks = firstTick:lastTick
  nticks = length(ticks)
  npersons = length(persnames)
  npropns = length(propnames)

  # Create version of the array we want, but with the inner matrices
  # defined by first two dimensions flipped along the diagonal.  
  # Seems to be the only easy way to do it.
  # See notes below for further explanation.
  flippedmats = array( t(dframe[firstTick:lastTick,]) ,  c(npropns,   npersons,  nticks) , 
                       dimnames=list(propositions=propnames, persons=persnames, ticks=ticks) )

  # Now return version with inner (i.e. first two) dimensions swapped,
  # so that the resulting array is what we want:
  aperm(flippedmats, c(2, 1, 3) )
}
