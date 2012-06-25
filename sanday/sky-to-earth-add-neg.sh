#!/bin/sh

numRunsPerParams=20
addlTicks=1500

cd $HOME/popco

###################################
extraPersons=0
echo
echo runs with $extraPersons extra persons
echo

numToFlip=8
runNum=YO
#while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/earth-to-sky-add-neg-runs.lisp $extraPersons $addlTicks $numToFlip "e2sAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
#        runNum=$(($runNum+1))
#done
