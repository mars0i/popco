#!/bin/sh
date
git branch
echo from github repository
echo settling prevented
run=75; time while [ $run -gt 0 ] ; do sbcl --eval '(progn (defvar *data-dir* "../data/lateDecSeproadback/crime2b/nosettle") (defvar *min-pop-ticks-to-settle* nil))' --load start --load c/crime2b ; run=$((run-1)); done
run=75; time while [ $run -gt 0 ] ; do sbcl --eval '(progn (defvar *data-dir* "../data/lateDecSeproadback/crime2c/nosettle") (defvar *min-pop-ticks-to-settle* nil))' --load start --load c/crime2c ; run=$((run-1)); done
run=75; time while [ $run -gt 0 ] ; do sbcl --eval '(progn (defvar *data-dir* "../data/lateDecSeproadback/crime2d/nosettle") (defvar *min-pop-ticks-to-settle* nil))' --load start --load c/crime2d ; run=$((run-1)); done
date
git branch
echo from github repository
echo settling prevented
