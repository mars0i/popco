#!/bin/sh

if [ -z "$2" ]; then
  echo usage: $0 arrayame vector-of-csv-names
  echo arrayname will also be used as filename with .rdata added
  echo vector-of-csv-names should have no spaces
  exit 1
fi

arrayname="$1"
csvvec="$2"
filename="${arrayname}.rdata"

R --no-save << END
source("~/pop/R/df2ra.R")
$arrayname <- read2multirunRA($csvvec)
save($arrayname, file="$filename")
END
