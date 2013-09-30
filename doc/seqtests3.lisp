

; this one doesn't seem to be fully tail recursive either (sbcl is supposed to be t-r)
(defun ints-aux (start end acc)
  (if (>= start end)
    acc
    (ints-aux start (1- end) (cons (gentemp) acc))))

(defun ints (start end)
  (ints-aux start end nil))

(defconstant +len+ 100000)

(defparameter lis (ints 0 +len+))
(defparameter arr (make-array +len+ :element-type 'symbol :initial-contents lis))

(defun set-up (x) 
  (setf (get x 'foo) 'yow))

(defun set-foo (x) 
  (setf (get x 'foo) 'bar))

(defun set-foo-d (x) 
  (declare (symbol x))
  (setf (get x 'foo) 'bar))

(mapc #'set-up lis)

(format t "done initializing~%")

(gc :full t)
(time 
  (progn
    (mapc #'set-foo lis)
    (mapc #'set-foo lis)
    (mapc #'set-foo lis)
    (mapc #'set-foo lis)
    (mapc #'set-foo lis)
    (mapc #'set-foo lis)
    (mapc #'set-foo lis)
    (mapc #'set-foo lis)
    (mapc #'set-foo lis)
    (mapc #'set-foo lis)
    (format t "mapc set-foo:~%")))

(gc :full t)
(time 
  (progn
    (map nil #'set-foo lis)
    (map nil #'set-foo lis)
    (map nil #'set-foo lis)
    (map nil #'set-foo lis)
    (map nil #'set-foo lis)
    (map nil #'set-foo lis)
    (map nil #'set-foo lis)
    (map nil #'set-foo lis)
    (map nil #'set-foo lis)
    (map nil #'set-foo lis)
    (format t "map nil set-foo with lists~%")))

(gc :full t)
(time 
  (progn 
    (map nil #'set-foo arr)
    (map nil #'set-foo arr)
    (map nil #'set-foo arr)
    (map nil #'set-foo arr)
    (map nil #'set-foo arr)
    (map nil #'set-foo arr)
    (map nil #'set-foo arr)
    (map nil #'set-foo arr)
    (map nil #'set-foo arr)
    (map nil #'set-foo arr)
    (format t "map nil set-foo with arrays, no declarations~%")))

(gc :full t)
(time 
  (progn 
    (map nil #'set-foo (the (simple-array symbol (100000)) arr))
    (map nil #'set-foo (the (simple-array symbol (100000)) arr))
    (map nil #'set-foo (the (simple-array symbol (100000)) arr))
    (map nil #'set-foo (the (simple-array symbol (100000)) arr))
    (map nil #'set-foo (the (simple-array symbol (100000)) arr))
    (map nil #'set-foo (the (simple-array symbol (100000)) arr))
    (map nil #'set-foo (the (simple-array symbol (100000)) arr))
    (map nil #'set-foo (the (simple-array symbol (100000)) arr))
    (map nil #'set-foo (the (simple-array symbol (100000)) arr))
    (map nil #'set-foo (the (simple-array symbol (100000)) arr))
    (format t "map nil set-foo with arrays, with declarations~%")))

(gc :full t)
(time 
  (progn
    (mapc #'set-foo-d lis)
    (mapc #'set-foo-d lis)
    (mapc #'set-foo-d lis)
    (mapc #'set-foo-d lis)
    (mapc #'set-foo-d lis)
    (mapc #'set-foo-d lis)
    (mapc #'set-foo-d lis)
    (mapc #'set-foo-d lis)
    (mapc #'set-foo-d lis)
    (mapc #'set-foo-d lis)
    (format t "mapc set-foo-d:~%")))

(gc :full t)
(time 
  (progn
    (map nil #'set-foo-d lis)
    (map nil #'set-foo-d lis)
    (map nil #'set-foo-d lis)
    (map nil #'set-foo-d lis)
    (map nil #'set-foo-d lis)
    (map nil #'set-foo-d lis)
    (map nil #'set-foo-d lis)
    (map nil #'set-foo-d lis)
    (map nil #'set-foo-d lis)
    (map nil #'set-foo-d lis)
    (format t "map nil set-foo-d with lists~%")))

(gc :full t)
(time 
  (progn 
    (map nil #'set-foo-d arr)
    (map nil #'set-foo-d arr)
    (map nil #'set-foo-d arr)
    (map nil #'set-foo-d arr)
    (map nil #'set-foo-d arr)
    (map nil #'set-foo-d arr)
    (map nil #'set-foo-d arr)
    (map nil #'set-foo-d arr)
    (map nil #'set-foo-d arr)
    (map nil #'set-foo-d arr)
    (format t "map nil set-foo-d with arrays, no declarations~%")))

(gc :full t)
(time 
  (progn 
    (map nil #'set-foo-d (the (simple-array symbol (100000)) arr))
    (map nil #'set-foo-d (the (simple-array symbol (100000)) arr))
    (map nil #'set-foo-d (the (simple-array symbol (100000)) arr))
    (map nil #'set-foo-d (the (simple-array symbol (100000)) arr))
    (map nil #'set-foo-d (the (simple-array symbol (100000)) arr))
    (map nil #'set-foo-d (the (simple-array symbol (100000)) arr))
    (map nil #'set-foo-d (the (simple-array symbol (100000)) arr))
    (map nil #'set-foo-d (the (simple-array symbol (100000)) arr))
    (map nil #'set-foo-d (the (simple-array symbol (100000)) arr))
    (map nil #'set-foo-d (the (simple-array symbol (100000)) arr))
    (format t "map nil set-foo-d with arrays, with declarations~%")))
