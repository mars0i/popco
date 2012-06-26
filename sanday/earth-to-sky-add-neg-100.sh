#!/bin/sh

numRunsPerParams=1
addlTicks=2000

cd $HOME/popco

###################################
extraPersons=91  # there are 9 propositions in earth-origin, i.e. nine non-naive person
echo
echo running 100-person population with all flipping, including neg flip
echo

numToFlip=100
runNum=1
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/earth-to-sky-add-neg-runs.lisp $extraPersons $addlTicks $numToFlip "e2sAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done
