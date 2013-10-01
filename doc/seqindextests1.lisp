

; this one doesn't seem to be fully tail recursive either (sbcl is supposed to be t-r)
(defun ints-aux (start end acc)
  (if (>= start end)
    acc
    (ints-aux start (1- end) (cons (gentemp) acc))))

(defun ints (start end)
  (ints-aux start end nil))

(defun set-up (x) 
  (setf (get x 'foo) 'yow))

(defconstant +len+ 100000)

(defparameter lis (ints 0 +len+))
(mapc #'set-up lis)
(defparameter arr (make-array +len+ :element-type 'symbol :initial-contents lis))

(gc :full t)
(time 
  (progn
    (dotimes (ignored 10000)
      (setf (elt lis (random +len+)) 'hey-lis))
    (format t "lis~%")))

(gc :full t)
(time 
  (progn
    (dotimes (ignored 10000)
      (setf (elt arr (random +len+)) 'hey-arr))
    (format t "arr~%")))
