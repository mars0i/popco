#!/bin/sh

if [ -z "$1" ]; then
	echo usage: $0 number-to-flip
	exit 1
fi

numRunsPerParams=1
addlTicks=50

cd $HOME/popco

###################################
extraPersons=9
extraPersonsToReport=9

numToFlip=$1
runNum=1
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/to-earth-immediately.lisp $extraPersons $addlTicks $numToFlip "YOeAddNeg${extraPersonsToReport}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done
