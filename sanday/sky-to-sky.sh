#!/bin/sh

numRunsPerParams=5
addlTicks=2000

cd $HOME/popco

###################################
extraPersons=0
echo
echo runs with $extraPersons extra persons
echo

numToFlip=0
runNum=1
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/sky-to-sky-runs.lisp $extraPersons $addlTicks $numToFlip "s2sAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done


###################################
extraPersons=10
echo
echo runs with $extraPersons extra persons
echo

numToFlip=0
runNum=1
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/sky-to-sky-runs.lisp $extraPersons $addlTicks $numToFlip "s2sAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done

###################################
extraPersons=92
echo
echo runs with $extraPersons extra persons
echo

numRunsPerParams=1
numToFlip=0
runNum=1
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/sky-to-sky-runs.lisp $extraPersons $addlTicks $numToFlip "s2sAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done
