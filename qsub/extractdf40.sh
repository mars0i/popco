#!/bin/sh

R --no-save << END
source("~/pop/R/df2ra.R")
load("huntingsoc.rdata")
huntingRun$1.df <- multiRA2PersonPropnDF(huntingsoc[,,,$1, drop=F], interval=40)
save(huntingRun$1.df, file="huntingRun${1}df.rdata")
END
