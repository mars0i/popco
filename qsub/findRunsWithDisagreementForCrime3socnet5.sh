#!/bin/sh

if [ -z "$2" ]; then
	echo usage: $0 basedatastruname tick [tick ...] 2>&1
	echo e.g.: $0 socnet5sh 4000 5000
	exit 1
fi

base="$1"
shift
ticks="$@"


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

load("$base.mra.rdata")

dimnames($base.mra)[-3]

for (tick in c($ticks)) {
  findruns($base.mra, tick)
}
END
