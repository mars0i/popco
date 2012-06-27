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
        time sbcl.executable --script sanday/sky-to-earth-add-neg-runs-no-flip.lisp $extraPersons $addlTicks $numToFlip "s2eAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
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
        time sbcl.executable --script sanday/sky-to-earth-add-neg-runs-no-flip.lisp $extraPersons $addlTicks $numToFlip "s2eAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done
