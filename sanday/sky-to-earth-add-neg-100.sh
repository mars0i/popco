#!/bin/sh

if [ -z "$1" ]; then
	echo usage: $0 number-to-flip
	exit 1
fi

numRunsPerParams=5
addlTicks=10000

cd $HOME/popco

###################################
extraPersons=91  # there are 9 propositions in earth-origin, i.e. nine non-naive person
echo
echo running 100-person population with flipping, including neg flip
echo

numToFlip=$1
runNum=5
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/sky-to-earth-add-neg.lisp $extraPersons $addlTicks $numToFlip "s2eAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done
