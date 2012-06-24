
(unlock-package 'common-lisp)
(load "start")
(load "sanday/parenting18untilCollect1")

; sb-ext:*posix-arv* contains the name of the executable and the command line args
; in the form of strings, which have not been eaten by the sbcl-option digestion process.

(unless (= 5 (length sb-ext:*posix-argv*))
  (format t "Usage: sbcl --script earth-to-sky-add-neg-runs.lisp num-extra-persons addl-ticks num-to-flip output-basename~%")
  (quit))

(let ((num-extra-persons (read-from-string (elt sb-ext:*posix-argv* 1)))
      (addl-ticks (read-from-string (elt sb-ext:*posix-argv* 1)))
      (num-to-flip (read-from-string (elt sb-ext:*posix-argv* 1)))
      (output-basename (elt sb-ext:*posix-argv* 1)))
  (format t "Running (earth-to-sky-add-neg ~S ~S ~S ~S)~%" num-extra-persons addl-ticks num-to-flip output-basename)
  (earth-to-sky-add-neg num-extra-persons addl-ticks num-to-flip output-basename))
