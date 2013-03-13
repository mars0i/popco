#!/bin/sh


R --no-save << END
source("~/pop/R/df2ra.R")

load("socnet5s.mra.rdata")

# verify that population structure is what we think:
dimnames(socnet5s.mra)[-3]

# normal two-subpop/bias dataframe
cat("beast: 2:21, virus: 22:41 ...\n")
socnet5s.df <- multiRAs2combinedMeanDF(list(socnet5s.mra[2:21,,,], socnet5s.mra[22:41,,,]), c("beast", "virus"), "CV", "CB")

# dataframe that splits up those who talk to the other subpop, from those that don't:
cat("beast, no talk: 2:15, talk: 16:21; virus, no talk: 22:35, talk: 36:41 ...\n")
# multiRAs2combinedMeanDF is supposed to be able to handle more than two mra's, but it has a bug, so we do it incrementally.
socnet5sSplitBeast.df <- multiRAs2combinedMeanDF(list(socnet5s.mra[2:15,,,], socnet5s.mra[16:21,,,]), c("beast", "beast-talk"), "CV", "CB")
socnet5sSplitVirus.df <- multiRAs2combinedMeanDF(list(socnet5s.mra[22:35,,,], socnet5s.mra[36:41,,,]), c("virus", "virus-talk"), "CV", "CB")
socnet5sSplit.df <- rbind(socnet5sSplitBeast.df, socnet5sSplitVirus.df )

save(socnet5s.df, socnet5sSplit.df, file="socnet5sdf.rdata")
END
