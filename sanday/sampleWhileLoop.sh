while [ $i -gt 0 ]; do sbcl.executable --script s/parenting18analogsAsBiases1iRun.lisp ; i=$((i-1)); done

while [ $i -gt 0 ]; do echo ; echo run $i: echo ; sbcl.executable --script s/parenting18analogsAsBiases2aRun.lisp ; i=$((i-1));  done

while [ $i -gt 0 ]; do echo ; echo run $i: ; sbcl.executable --script s/parenting18analogsAsBiases2aRun.lisp ; i=$((i-1)); done

[first use ln -s to give directories of interest aliases both, sky, earth, then:]
for f in both sky earth; do cd $f ; pwd ; Rscript ~/pop/R/analogsAsBiases2.R $f ; cd .. ; pwd ; done
[Note that the alias is passed to the R script; it gets used as part of the plot label.]
