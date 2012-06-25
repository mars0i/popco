; sky-to-earth-add-neg-runs.lisp
; setup script for specific run scripts

(defvar *run-fn-name* 'sky-to-earth-add-neg) ; change this for different script behavior

#+sbcl (unlock-package 'common-lisp)

; next lines set everything up
(load "start") ; popco
(load "sanday/parenting18untilCollect1")  ; library of functions that define persons, run popoco, etc.

(defvar *run-fn* #'sky-to-earth-add-neg) ; change this for different script behavior

; get parameters from the command line:
(defvar *argv* 'uh-oh)
#+sbcl (setf *argv* (cdr *posix-argv*)) ; in sbcl, do this
#+abcl (setf *argv* *command-line-argument-list*) ; in abcl, do this
; if not in sbcl, and if not in abcl, we don't know what to do:
#-sbcl #-abcl (error "This Lisp implementation doesn't seem to have the functionality we need.")
(unless (= 4 (length *argv*))
  #+sbcl (format *error-output* "Usage: ~A --script ~(~A~)-runs.lisp num-extra-persons addl-ticks num-to-flip output-basename~%" (car *posix-argv*) *run-fn-name*)
  #+abcl (format *error-output* "Usage: abcl --batch --load ~(~A~)-runs.lisp num-extra-persons addl-ticks num-to-flip output-basename~%" *run-fn-name*)
  (quit))

(let ((num-extra-persons (read-from-string (first *argv*)))
      (addl-ticks (read-from-string (second *argv*)))
      (num-to-flip (read-from-string (third *argv*)))
      (output-basename (fourth *argv*)))
  (format t "Running (funcall ~S ~S ~S ~S ~S)~%" *run-fn-name* num-extra-persons addl-ticks num-to-flip output-basename)
  (funcall *run-fn-name* num-extra-persons addl-ticks num-to-flip output-basename))
