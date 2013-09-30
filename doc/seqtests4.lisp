

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

(defun get-foo (x) 
  (get x 'foo))

(defun get-foo-d (x) 
  (declare (symbol x))
  (get x 'foo))

; initialize the properties first.  otherwise the first time trial has the overhead of the initial conses.
(mapc #'set-up lis)

(format t "done initializing~%")

(gc :full t)
(time 
  (progn
    (mapc #'get-foo lis)
    (mapc #'get-foo lis)
    (mapc #'get-foo lis)
    (mapc #'get-foo lis)
    (mapc #'get-foo lis)
    (mapc #'get-foo lis)
    (mapc #'get-foo lis)
    (mapc #'get-foo lis)
    (mapc #'get-foo lis)
    (mapc #'get-foo lis)
    (format t "mapc get-foo:~%")))

(gc :full t)
(time 
  (progn
    (map nil #'get-foo lis)
    (map nil #'get-foo lis)
    (map nil #'get-foo lis)
    (map nil #'get-foo lis)
    (map nil #'get-foo lis)
    (map nil #'get-foo lis)
    (map nil #'get-foo lis)
    (map nil #'get-foo lis)
    (map nil #'get-foo lis)
    (map nil #'get-foo lis)
    (format t "map nil get-foo with lists~%")))

(gc :full t)
(time 
  (progn 
    (map nil #'get-foo arr)
    (map nil #'get-foo arr)
    (map nil #'get-foo arr)
    (map nil #'get-foo arr)
    (map nil #'get-foo arr)
    (map nil #'get-foo arr)
    (map nil #'get-foo arr)
    (map nil #'get-foo arr)
    (map nil #'get-foo arr)
    (map nil #'get-foo arr)
    (format t "map nil get-foo with arrays, no declarations~%")))

(gc :full t)
(time 
  (progn 
    (map nil #'get-foo (the (simple-array symbol (100000)) arr))
    (map nil #'get-foo (the (simple-array symbol (100000)) arr))
    (map nil #'get-foo (the (simple-array symbol (100000)) arr))
    (map nil #'get-foo (the (simple-array symbol (100000)) arr))
    (map nil #'get-foo (the (simple-array symbol (100000)) arr))
    (map nil #'get-foo (the (simple-array symbol (100000)) arr))
    (map nil #'get-foo (the (simple-array symbol (100000)) arr))
    (map nil #'get-foo (the (simple-array symbol (100000)) arr))
    (map nil #'get-foo (the (simple-array symbol (100000)) arr))
    (map nil #'get-foo (the (simple-array symbol (100000)) arr))
    (format t "map nil get-foo with arrays, with declarations~%")))

(gc :full t)
(time 
  (progn
    (mapc #'get-foo-d lis)
    (mapc #'get-foo-d lis)
    (mapc #'get-foo-d lis)
    (mapc #'get-foo-d lis)
    (mapc #'get-foo-d lis)
    (mapc #'get-foo-d lis)
    (mapc #'get-foo-d lis)
    (mapc #'get-foo-d lis)
    (mapc #'get-foo-d lis)
    (mapc #'get-foo-d lis)
    (format t "mapc get-foo-d:~%")))

(gc :full t)
(time 
  (progn
    (map nil #'get-foo-d lis)
    (map nil #'get-foo-d lis)
    (map nil #'get-foo-d lis)
    (map nil #'get-foo-d lis)
    (map nil #'get-foo-d lis)
    (map nil #'get-foo-d lis)
    (map nil #'get-foo-d lis)
    (map nil #'get-foo-d lis)
    (map nil #'get-foo-d lis)
    (map nil #'get-foo-d lis)
    (format t "map nil get-foo-d with lists~%")))

(gc :full t)
(time 
  (progn 
    (map nil #'get-foo-d arr)
    (map nil #'get-foo-d arr)
    (map nil #'get-foo-d arr)
    (map nil #'get-foo-d arr)
    (map nil #'get-foo-d arr)
    (map nil #'get-foo-d arr)
    (map nil #'get-foo-d arr)
    (map nil #'get-foo-d arr)
    (map nil #'get-foo-d arr)
    (map nil #'get-foo-d arr)
    (format t "map nil get-foo-d with arrays, no declarations~%")))

(gc :full t)
(time 
  (progn 
    (map nil #'get-foo-d (the (simple-array symbol (100000)) arr))
    (map nil #'get-foo-d (the (simple-array symbol (100000)) arr))
    (map nil #'get-foo-d (the (simple-array symbol (100000)) arr))
    (map nil #'get-foo-d (the (simple-array symbol (100000)) arr))
    (map nil #'get-foo-d (the (simple-array symbol (100000)) arr))
    (map nil #'get-foo-d (the (simple-array symbol (100000)) arr))
    (map nil #'get-foo-d (the (simple-array symbol (100000)) arr))
    (map nil #'get-foo-d (the (simple-array symbol (100000)) arr))
    (map nil #'get-foo-d (the (simple-array symbol (100000)) arr))
    (map nil #'get-foo-d (the (simple-array symbol (100000)) arr))
    (format t "map nil get-foo-d with arrays, with declarations~%")))
