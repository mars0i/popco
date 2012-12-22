; Running this shows whether reporting pop-ticks to the console takes significant
; time in a given implementation of Common Lisp

(defvar *pop-tick* 0)

(defun report-progress-to-console ()
  (format t "~C~C~C~C~C~C~C~C~C~S" #\backspace #\backspace #\backspace #\backspace #\backspace #\backspace #\backspace #\backspace #\backspace
                                         *pop-tick*))
(defun no-op () t)

(defun report-loop ()
  (time
    (dotimes (i 10000)
      (setf *pop-tick* i)
      (report-progress-to-console))))

(defun no-op-loop ()
  (time
    (dotimes (i 10000)
      (setf *pop-tick* i)
      (no-op))))

