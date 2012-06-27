#!/bin/sh

lispscript=growth-of-earth.lisp

addlTicks=3000
numToFlip=0

cd $HOME/popco

# ###################################
# extraPersons=0
# numRunsPerParams=5
# echo
# echo runs with $extraPersons extra persons
# echo
# 
# runNum=1
# while [ $runNum -le $numRunsPerParams ]; do
#         time sbcl.executable --script sanday/$lispscript $extraPersons $addlTicks "growEarth${extraPersons}extra${addlTicks}addlRun$runNum"
#         runNum=$(($runNum+1))
# done
# 
# 
# ###################################
# extraPersons=11
# numRunsPerParams=5
# echo
# echo runs with $extraPersons extra persons
# echo
# 
# runNum=1
# while [ $runNum -le $numRunsPerParams ]; do
#         time sbcl.executable --script sanday/$lispscript $extraPersons $addlTicks "growEarth${extraPersons}extra${addlTicks}addlRun$runNum"
#         runNum=$(($runNum+1))
# done
# 
# ###################################
# extraPersons=21
# numRunsPerParams=2
# echo
# echo runs with $extraPersons extra persons
# echo
# 
# runNum=1
# while [ $runNum -le $numRunsPerParams ]; do
#         time sbcl.executable --script sanday/$lispscript $extraPersons $addlTicks "growEarth${extraPersons}extra${addlTicks}addlRun$runNum"
#         runNum=$(($runNum+1))
# done

###################################
extraPersons=91
numRunsPerParams=2
echo
echo runs with $extraPersons extra persons
echo

runNum=1
while [ $runNum -le $numRunsPerParams ]; do
        time sbcl.executable --script sanday/$lispscript $extraPersons $addlTicks "growEarth${extraPersons}extra${addlTicks}addlRun$runNum"
        runNum=$(($runNum+1))
done
