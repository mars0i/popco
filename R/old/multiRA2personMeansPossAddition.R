multiRA2personMeansRA <- function(multiRA, doms, lastTick=dim(multiRA)[3], firstTick=lastTick) {
  mra <- stripRunPaths(multiRA)

  numdoms <- length(doms)
  meanmra.dims <- dim(mra)
  meanmra.dims[2] <- numdoms # replace number of propns with number of doms
  meanmra <- array(0, meanmra.dims)

  for (i in numdoms) {
    # c(1,3,4) as an argument to apply means average *only* over propositions 
    # (within the domain), not over persons (1), ticks (3), or runs (4):
    meanmra[,i,,] <- apply(multiRA2punditFreeDomRA(mra[,,firstTick:lastTick,,drop=F], doms[i]), c(1,3,4), mean)
  }

  dimnames(meanmra) <- list(person=dimnames(meanmra)[[1]],
                            dom=dimnames(meanmra)[[2]],
			    tick=dimnames(meanmra)[[3]],
			    run=dimnames(meanmra)[[4]])

  meanmra
}
