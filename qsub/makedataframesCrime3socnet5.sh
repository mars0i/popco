#!/bin/sh


R --no-save << END
source("~/pop/R/df2ra.R")

load("socnet5s.mra.rdata")

dimnames(socnet5s.mra)[-3]

END
