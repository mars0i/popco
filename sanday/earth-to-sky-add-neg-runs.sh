#!/bin/sh

numRunsPerParams=20
addlTicks=1500

cd $HOME/popco

###################################
extraPersons=0
echo
echo runs with $extraPersons extra persons
echo

numToFlip=1
runNum=1
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/earth-to-sky-add-neg-runs.lisp $extraPersons $addlTicks $numToFlip "e2sAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done

numToFlip=4
runNum=1
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/earth-to-sky-add-neg-runs.lisp $extraPersons $addlTicks $numToFlip "e2sAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done

numToFlip=8
runNum=1
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/earth-to-sky-add-neg-runs.lisp $extraPersons $addlTicks $numToFlip "e2sAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done


###################################
extraPersons=10
echo
echo runs with $extraPersons extra persons
echo

numToFlip=1
runNum=1
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/earth-to-sky-add-neg-runs.lisp $extraPersons $addlTicks $numToFlip "e2sAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done

numToFlip=4
runNum=1
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/earth-to-sky-add-neg-runs.lisp $extraPersons $addlTicks $numToFlip "e2sAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done

numToFlip=8
runNum=1
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/earth-to-sky-add-neg-runs.lisp $extraPersons $addlTicks $numToFlip "e2sAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done

numToFlip=18
runNum=1
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/earth-to-sky-add-neg-runs.lisp $extraPersons $addlTicks $numToFlip "e2sAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done

###################################
extraPersons=30
echo
echo runs with $extraPersons extra persons
echo

numToFlip=1
runNum=1
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/earth-to-sky-add-neg-runs.lisp $extraPersons $addlTicks $numToFlip "e2sAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done

numToFlip=4
runNum=1
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/earth-to-sky-add-neg-runs.lisp $extraPersons $addlTicks $numToFlip "e2sAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done

numToFlip=8
runNum=1
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/earth-to-sky-add-neg-runs.lisp $extraPersons $addlTicks $numToFlip "e2sAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done

numToFlip=20
runNum=1
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/earth-to-sky-add-neg-runs.lisp $extraPersons $addlTicks $numToFlip "e2sAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done

numToFlip=30
runNum=1
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/earth-to-sky-add-neg-runs.lisp $extraPersons $addlTicks $numToFlip "e2sAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done

###################################
extraPersons=90
echo
echo runs with $extraPersons extra persons
echo

numToFlip=1
runNum=1
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/earth-to-sky-add-neg-runs.lisp $extraPersons $addlTicks $numToFlip "e2sAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done

numToFlip=4
runNum=1
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/earth-to-sky-add-neg-runs.lisp $extraPersons $addlTicks $numToFlip "e2sAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done

numToFlip=8
runNum=1
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/earth-to-sky-add-neg-runs.lisp $extraPersons $addlTicks $numToFlip "e2sAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done

numToFlip=20
runNum=1
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/earth-to-sky-add-neg-runs.lisp $extraPersons $addlTicks $numToFlip "e2sAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done

numToFlip=50
runNum=1
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/earth-to-sky-add-neg-runs.lisp $extraPersons $addlTicks $numToFlip "e2sAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done

numToFlip=80
runNum=1
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/earth-to-sky-add-neg-runs.lisp $extraPersons $addlTicks $numToFlip "e2sAddNeg${extraPersons}extra${addlTicks}addl${numToFlip}flippedRun$runNum"
        runNum=$(($runNum+1))
done


echo
echo DONE
echo 
