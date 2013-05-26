; to-earth-immediately.lisp
; JUST LIKE sky-to-earth-add-neg.lisp, BUT MAKES THE SWITCHOVER IMMEDIATELY.
; WE DO THIS BY ADDING AN ADDITIONAL PERSON WHO HAS ALL OF THE PROPNS THAT 
; WOULD HAVE TO BE COLLECTED OTHERWISE.
#-sbcl (error "This script is designed to run in SBCL, not other Common Lisps.")
(unlock-package 'common-lisp)
(load "start") ; popco
(load "sanday/parenting18untilCollect5")  ; library of functions that define persons, run popoco, etc.

(defvar *run-fn-name* 'sky-to-earth-add-neg) ; change this for different script behavior
(defvar *run-fn* #'sky-to-earth-add-neg) ; change this for different script behavior

; get parameters from the command line:
(defvar *argv* (cdr *posix-argv*)) ; in sbcl, do this

(unless (= 4 (length *argv*))
  (format *error-output* "Usage: ~A --script ~(~A~)-runs.lisp num-extra-persons addl-ticks num-to-flip output-basename~%" (car *posix-argv*) *run-fn-name*)
  (quit))

;; MAKE THE EXTRA PERSON
(make-skyless-hunter 'sh)

(let ((num-extra-persons (read-from-string (first *argv*)))
      (addl-ticks (read-from-string (second *argv*)))
      (num-to-flip (read-from-string (third *argv*)))
      (output-basename (fourth *argv*)))
  (format t "Running (funcall ~S ~S ~S ~S ~S)~%" *run-fn-name* num-extra-persons addl-ticks num-to-flip output-basename)
  (funcall *run-fn-name* num-extra-persons addl-ticks num-to-flip output-basename))
