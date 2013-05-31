#!/bin/sh

if [ -z "$3" ]; then
  echo usage: $0 oldmra newmra dom1, dom2, ...
  exit 1
fi

oldmra="$1"
shift
newmra="$1"
shift
doms="$@"
doms=`echo $doms | sed -e 's/^/c("/'   -e 's/ /","/g'   -e 's/$/")/'`

datadir=`pwd`

R --no-save << END
source("~/pop/R/df2ra.R")
setwd("$datadir")
load("$oldmra.rdata")
$newmra <-multiRA2domMeanRA($oldmra, $doms)
save($newmra, file="$newmra.rdata")
END
