#!/bin/sh

if [ -z "$1" ]; then
	echo usage: $0 number-to-flip
	exit 1
fi

numRunsPerParams=2
addlTicks=10000

cd $HOME/popco

###################################
extraPersons=92  # there are 8 propositions in sky-origin, i.e. nine non-naive person
echo
echo running 100-person population with flipping, including neg flip
echo

numToFlip=$1
runNum=2
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/earth-to-sky-add-neg.lisp $extraPersons $addlTicks $numToFlip "e2sAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done
