; earth-to-sky-add-neg-runs.lisp

#+sbcl (unlock-package 'common-lisp)

; next liness sets everything up
(load "start")
(load "sanday/parenting18untilCollect1")
(load "sanday/parenting18untilCollect1runs")

(defvar *run-fn* #'earth-to-sky-add-neg) ; change this for different script behavior

(format t "Running (funcall ~S ~S ~S ~S ~S)~%" *run-fn* *num-extra-persons* *addl-ticks* *num-to-flip* *output-basename*)
(funcall *run-fn* *num-extra-persons* *addl-ticks* *num-to-flip* *output-basename*)
