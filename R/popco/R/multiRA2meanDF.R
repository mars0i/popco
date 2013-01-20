multiRA2meanDF <-
function(multiRA, dom1, dom2, lastTick=dim(multiRA)[3], firstTick=lastTick) {
  mra <- stripRunPaths(multiRA)
  df <- data.frame(apply(multiRA2punditFreeDomRA(mra[,,firstTick:lastTick,,drop=F], dom1), 4, mean),
                   apply(multiRA2punditFreeDomRA(mra[,,firstTick:lastTick,,drop=F], dom2), 4, mean))
  names(df) <- c(dom1, dom2)
  df$rawsum <- "raw"
  df <- rbind(df, c(colMeans(df[,1:2]), rawsum="mean"))
  # One of the steps above--not sure which--coerces the numbers to strings; undo that change:
  df[[dom1]] <- as.numeric(df[[dom1]])
  df[[dom2]] <- as.numeric(df[[dom2]])
  df
}
