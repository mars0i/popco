#!/bin/sh

if [ -z "$1" ]; then
	echo usage: $0 basedatastruname 2>&1
	exit 1
fi

base="$1"

R --no-save << END
source("~/pop/R/df2ra.R")

load("$base.mra.rdata")

# verify that population structure is what we think:
dimnames($base.mra)[-3]

# normal two-subpop/bias dataframe
cat("beast: 2:21, virus: 22:41 ...\n")
$base.df <- multiRAs2combinedMeanDF(list($base.mra[2:21,,,], $base.mra[22:41,,,]), c("beast", "virus"), "CV", "CB")

# dataframe that splits up those who talk to the other subpop, from those that don't:
cat("beast, no talk: 2:15, talk: 16:21; virus, no talk: 22:35, talk: 36:41 ...\n")
# multiRAs2combinedMeanDF is supposed to be able to handle more than two mra's, but it has a bug, so we do it incrementally.
${base}SplitBeast.df <- multiRAs2combinedMeanDF(list($base.mra[2:15,,,], $base.mra[16:21,,,]), c("beast", "beast-talk"), "CV", "CB")
${base}SplitVirus.df <- multiRAs2combinedMeanDF(list($base.mra[22:35,,,], $base.mra[36:41,,,]), c("virus", "virus-talk"), "CV", "CB")
${base}Split.df <- rbind(${base}SplitBeast.df, ${base}SplitVirus.df )

save($base.df, ${base}Split.df, file="$base.df.rdata")
END
