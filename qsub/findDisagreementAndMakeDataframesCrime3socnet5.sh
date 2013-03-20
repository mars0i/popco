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

# verify that population structure is what we think:
dimnames($base.mra)[-3]

# send output into the log file that verifies whether propn activns have converged within each run:
for (tick in c($ticks)) {
  findruns($base.mra, tick)
}

# make normal two-subpop/bias dataframe
cat("beast: 2:21, virus: 22:41 ...\n")
$base.df <- multiRAs2combinedMeanDF(list($base.mra[2:21,,,], $base.mra[22:41,,,]), c("beast", "virus"), "CV", "CB")

# make dataframe that splits up those who talk to the other subpop, from those that don't:
cat("beast, no talk: 2:15, talk: 16:21; virus, no talk: 22:35, talk: 36:41 ...\n")

# multiRAs2combinedMeanDF is supposed to be able to handle more than two mra's, but it has a bug, so we'll do in two steps:
${base}SplitBeast.df <- multiRAs2combinedMeanDF(list($base.mra[2:15,,,], $base.mra[16:21,,,]), c("beast", "beast-talk"), "CV", "CB")
${base}SplitVirus.df <- multiRAs2combinedMeanDF(list($base.mra[22:35,,,], $base.mra[36:41,,,]), c("virus", "virus-talk"), "CV", "CB")

# now combine the two partial dataframes into one big dataframe:
${base}Split.df <- rbind(${base}SplitBeast.df, ${base}SplitVirus.df )

# all done--we can save the new dataframes
save($base.df, ${base}Split.df, file="$base.df.rdata")

END
