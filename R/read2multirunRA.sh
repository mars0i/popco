#!/bin/sh

if [ -z "$3" ]; then
  echo usage: $0 datadir arrayame vector-of-csv-names
  echo datadir is a relative path wrt the default data dir--probably ../data wrt the popco dir
  echo arrayname will also be used as filename with .rdata added
  echo vector-of-csv-names should have no spaces
  exit 1
fi

datadir="$1"
arrayname="$2"
csvvec="$3"
filename="${arrayname}.rdata"

R --no-save << END
source("~/pop/R/df2ra.R")
setwd("$datadir")
$arrayname <- read2multirunRA($csvvec)
save($arrayname, file="$filename")
END
