#!/bin/sh


R --no-save << END
source("~/pop/R/df2ra.R")

findruns <- function(mra, tick) {
  print(paste0("\ntick",tick,"\nbad runs:"))
  print("beast:")
  print(findRunsWithDisagreement(mra[2:21,,,], .1, tickIndex=tick))
  print("virus:")
  print(findRunsWithDisagreement(mra[22:41,,,], .1, tickIndex=tick))
  print("beast no talk:")
  print(findRunsWithDisagreement(mra[2:15,,,], .1, tickIndex=tick))
  print("beast talk:")
  print(findRunsWithDisagreement(mra[16:21,,,], .1, tickIndex=tick))
  print("virus no talk:")
  print(findRunsWithDisagreement(mra[22:35,,,], .1, tickIndex=tick))
  print("virus talk:")
  print(findRunsWithDisagreement(mra[36:41,,,], .1, tickIndex=tick))
  print("\n")
}

load("socnet5s.mra.rdata")

dimnames(socnet5s.mra)[-3]

for (tick in c(5000,6000,7000,8000)) {
  findruns(socnet5s.mra, tick)
}
END
