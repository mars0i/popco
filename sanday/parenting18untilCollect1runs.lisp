; parenting18untilCollect1runs.lisp
; setup script for specific run scripts

#-sbcl (unlock-package 'common-lisp)

(defvar *argv*)

; if in sbcl, do this:
#+sbcl (setf *argv* (cdr *argv*))
; if not in sbcl, but in abcl, do this:
#-sbcl #+abcl (setf *argv* *command-line-argument-list*)
; if in neither sbcl nor abcl, do this:
#-sbcl #-abcl (error "This Lisp script uses SBCL-specific features.  It's unlikely to run without modification in other Lisps.")

(unless (= 4 (length *argv*))
  #+sbcl (format t "Usage: sbcl --script ~S-runs.lisp num-extra-persons addl-ticks num-to-flip output-basename~%" *run-fn*)
  #+abcl (format t "Usage: abcl --batch --load ~S-runs.lisp num-extra-persons addl-ticks num-to-flip output-basename~%" *run-fn*)
  (quit))

(setf *num-extra-persons* (read-from-string (elt *argv* 0)))
(setf *addl-ticks* (read-from-string (elt *argv* 1)))
(setf *num-to-flip* (read-from-string (elt *argv* 2)))
(setf *output-basename* (elt *argv* 3))
