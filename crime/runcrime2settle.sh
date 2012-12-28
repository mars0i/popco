run=75; time while [ $run -gt 0 ] ; do sbcl --eval '(defvar *data-dir* "../data/lateDecSeproadback/crime2b/settle")' --load start --load c/crime2b ; run=$((run-1)); done
run=75; time while [ $run -gt 0 ] ; do sbcl --eval '(defvar *data-dir* "../data/lateDecSeproadback/crime2c/settle")' --load start --load c/crime2c ; run=$((run-1)); done
run=75; time while [ $run -gt 0 ] ; do sbcl --eval '(defvar *data-dir* "../data/lateDecSeproadback/crime2d/settle")' --load start --load c/crime2d ; run=$((run-1)); done
