#!/bin/sh

csvvec=`echo *.csv | files2vec`
if [ -z "$csvvec" -o -z "$1" ]; then
	echo usage: $0 mra
	echo Note: Should be run from the directory containing the csvs you want to process.
	exit 1
fi

qsub ~/jobs/read2multirunRA.job `pwd` $1 "$csvvec"
