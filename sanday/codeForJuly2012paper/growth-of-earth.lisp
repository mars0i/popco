; grow-of-earth.lisp
; setup script for specific run scripts
#-sbcl (error "This script is designed to run in SBCL, not other Common Lisps.")
(unlock-package 'common-lisp)
(load "start") ; popco
(load "sanday/parenting18untilCollect5")  ; library of functions that define persons, run popoco, etc.

(defvar *run-fn-name* 'growth-of-earth-pop) ; change this for different script behavior
(defvar *run-fn* #'growth-of-earth-pop) ; change this for different script behavior

; get parameters from the command line:
(defvar *argv* (cdr *posix-argv*)) ; in sbcl, do this

(unless (= 3 (length *argv*))
  (format *error-output* "Usage: ~A --script ~(~A~)-runs.lisp num-extra-persons addl-ticks num-to-flip output-basename~%" (car *posix-argv*) *run-fn-name*)
  (quit))

(let ((num-extra-persons (read-from-string (first *argv*)))
      (addl-ticks (read-from-string (second *argv*)))
      (output-basename (third *argv*)))
  (format t "Running (funcall ~S ~S ~S ~S)~%" *run-fn-name* num-extra-persons addl-ticks output-basename)
  (funcall *run-fn-name* num-extra-persons addl-ticks output-basename))
