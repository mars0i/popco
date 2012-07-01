#!/bin/sh

if [ -z "$1" ]; then
	echo usage: $0 number-to-flip
	exit 1
fi

numRunsPerParams=2
addlTicks=5000

cd $HOME/popco

###################################
extraPersons=90  # there are 9 propositions in earth-origin, i.e. nine non-naive person, but we added one
extraPersonsToReport=91

numToFlip=$1
runNum=1
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/to-earth-immediately.lisp $extraPersons $addlTicks $numToFlip "eAddNeg${extraPersonsToReport}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done
