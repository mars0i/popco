#!/bin/sh

if [ -z "$3" ]; then
	echo usage $0 mra_name sample_interval run_num
	exit 1
fi

mra="$1"
interval="$2"
run="$3"

df=${mra}Run$run.df
infile=$mra.rdata
outfile=${mra}Run${run}df.rdata

R --no-save << END
source("~/pop/R/df2ra.R")
load("$infile")
$df <- multiRA2PersonPropnDF(${mra}[,,,$run, drop=F], interval=$interval)
save($df, file="$outfile")
END
