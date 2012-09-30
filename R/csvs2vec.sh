#!/bin/sh
# Given a series of filenames on a single line in stdin,
# e.g. produced via
#	echo *.csv | thisscript
# this script constructs the R command
#	csvs <- c("filename1","filename2",...,"filenameN")

sed -e 's/\([^ ][^ ]*\)/"\1",/g'  -e 's/\(.*\), */csvs <- c(\1)/'
