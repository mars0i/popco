# list contents of an R data file to stdout
# suggested usage:
# Rscript listFileContents.R blahblah.rdata > blahblah.lis

rm(setDisplayWidth, punditPrefix) # clean out my junk
filename <- commandArgs(TRUE)[1] # get R data file name passed on command line
load(filename)
cat(filename, ":\n", sep="") # display name of file to stdout
rm(filename) # remove filename var from environment
ls() # list variables that were in the file to stdout

