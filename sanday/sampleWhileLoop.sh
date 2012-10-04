while [ $i -gt 0 ]; do sbcl.executable --script s/parenting18analogsAsBiases1iRun.lisp ; i=$((i-1)); done
while [ $i -gt 0 ]; do echo ; echo run $i: echo ; sbcl.executable --script s/parenting18analogsAsBiases2aRun.lisp ; i=$((i-1));  done
time(while [ $i -gt 0 ]; do echo ; echo run $i: ; sbcl.executable --script s/parenting18analogsAsBiases2aRun.lisp ; i=$((i-1)); done)
