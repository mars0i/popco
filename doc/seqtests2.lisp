
;; non-tail recursive version:
;(defun ints (start end)
;  (if (>= start end)
;    nil
;    (cons start (ints (1+ start) end))))


; this one doesn't seem to be fully tail recursive, either (sbcl is supposed to be t-r)
(defun ints-aux (start end acc)
  (if (>= start end)
    acc
    (ints-aux start (1- end) (cons (1- end) acc))))

(defun ints (start end)
  (ints-aux start end (list end)))

(defconstant +len+ 100000)
(defconstant +len1+ (1+ +len+))

(defparameter lis (ints 0 +len+))
(defparameter vec (apply #'vector lis))
(defparameter arr (make-array +len1+ :element-type 'fixnum :initial-contents lis))

;(defconstant lis (ints 0 100000))
;(defconstant (ints 3 100003))
;(defconstant vec (apply #'vector lis))
;(defconstant (apply #'vector))

(format t "done initializing~%~%")

(format t "~%without declarations:~%~%")

(gc :full t)
(time 
  (progn
    (mapcar #'1+ lis)
    (mapcar #'1+ lis)
    (mapcar #'1+ lis)
    (mapcar #'1+ lis)
    (mapcar #'1+ lis)
    (mapcar #'1+ lis)
    (mapcar #'1+ lis)
    (mapcar #'1+ lis)
    (mapcar #'1+ lis)
    (mapcar #'1+ lis)
    (mapcar #'1+ lis)
    (mapcar #'1+ lis)
    (mapcar #'1+ lis)
    (mapcar #'1+ lis)
    (mapcar #'1+ lis)
    (mapcar #'1+ lis)
    (mapcar #'1+ lis)
    (mapcar #'1+ lis)
    (mapcar #'1+ lis)
    (mapcar #'1+ lis)
    (format t "mapcar:~%")))

(gc :full t)
(time 
  (progn
    (mapc #'1+ lis)
    (mapc #'1+ lis)
    (mapc #'1+ lis)
    (mapc #'1+ lis)
    (mapc #'1+ lis)
    (mapc #'1+ lis)
    (mapc #'1+ lis)
    (mapc #'1+ lis)
    (mapc #'1+ lis)
    (mapc #'1+ lis)
    (mapc #'1+ lis)
    (mapc #'1+ lis)
    (mapc #'1+ lis)
    (mapc #'1+ lis)
    (mapc #'1+ lis)
    (mapc #'1+ lis)
    (mapc #'1+ lis)
    (mapc #'1+ lis)
    (mapc #'1+ lis)
    (mapc #'1+ lis)
    (format t "mapc:~%")))

(gc :full t)
(time 
  (progn
    (map 'list #'1+ lis)
    (map 'list #'1+ lis)
    (map 'list #'1+ lis)
    (map 'list #'1+ lis)
    (map 'list #'1+ lis)
    (map 'list #'1+ lis)
    (map 'list #'1+ lis)
    (map 'list #'1+ lis)
    (map 'list #'1+ lis)
    (map 'list #'1+ lis)
    (map 'list #'1+ lis)
    (map 'list #'1+ lis)
    (map 'list #'1+ lis)
    (map 'list #'1+ lis)
    (map 'list #'1+ lis)
    (map 'list #'1+ lis)
    (map 'list #'1+ lis)
    (map 'list #'1+ lis)
    (map 'list #'1+ lis)
    (map 'list #'1+ lis)
    (format t "map list:~%")))

(gc :full t)
(time 
  (progn
    (map nil #'1+ lis)
    (map nil #'1+ lis)
    (map nil #'1+ lis)
    (map nil #'1+ lis)
    (map nil #'1+ lis)
    (map nil #'1+ lis)
    (map nil #'1+ lis)
    (map nil #'1+ lis)
    (map nil #'1+ lis)
    (map nil #'1+ lis)
    (map nil #'1+ lis)
    (map nil #'1+ lis)
    (map nil #'1+ lis)
    (map nil #'1+ lis)
    (map nil #'1+ lis)
    (map nil #'1+ lis)
    (map nil #'1+ lis)
    (map nil #'1+ lis)
    (map nil #'1+ lis)
    (map nil #'1+ lis)
    (format t "map nil with lists~%")))

(gc :full t)
(time 
  (progn 
    (map 'vector #'1+ vec)
    (map 'vector #'1+ vec)
    (map 'vector #'1+ vec)
    (map 'vector #'1+ vec)
    (map 'vector #'1+ vec)
    (map 'vector #'1+ vec)
    (map 'vector #'1+ vec)
    (map 'vector #'1+ vec)
    (map 'vector #'1+ vec)
    (map 'vector #'1+ vec)
    (map 'vector #'1+ vec)
    (map 'vector #'1+ vec)
    (map 'vector #'1+ vec)
    (map 'vector #'1+ vec)
    (map 'vector #'1+ vec)
    (map 'vector #'1+ vec)
    (map 'vector #'1+ vec)
    (map 'vector #'1+ vec)
    (map 'vector #'1+ vec)
    (map 'vector #'1+ vec)
    (format t "map vector:~%")))

(gc :full t)
(time 
  (progn 
    (map nil #'1+ vec)
    (map nil #'1+ vec)
    (map nil #'1+ vec)
    (map nil #'1+ vec)
    (map nil #'1+ vec)
    (map nil #'1+ vec)
    (map nil #'1+ vec)
    (map nil #'1+ vec)
    (map nil #'1+ vec)
    (map nil #'1+ vec)
    (map nil #'1+ vec)
    (map nil #'1+ vec)
    (map nil #'1+ vec)
    (map nil #'1+ vec)
    (map nil #'1+ vec)
    (map nil #'1+ vec)
    (map nil #'1+ vec)
    (map nil #'1+ vec)
    (map nil #'1+ vec)
    (map nil #'1+ vec)
    (format t "map nil with vectors:~%")))

(gc :full t)
(time 
  (progn 
    (map '(simple-array fixnum (100001)) #'1+ arr)
    (map '(simple-array fixnum (100001)) #'1+ arr)
    (map '(simple-array fixnum (100001)) #'1+ arr)
    (map '(simple-array fixnum (100001)) #'1+ arr)
    (map '(simple-array fixnum (100001)) #'1+ arr)
    (map '(simple-array fixnum (100001)) #'1+ arr)
    (map '(simple-array fixnum (100001)) #'1+ arr)
    (map '(simple-array fixnum (100001)) #'1+ arr)
    (map '(simple-array fixnum (100001)) #'1+ arr)
    (map '(simple-array fixnum (100001)) #'1+ arr)
    (map '(simple-array fixnum (100001)) #'1+ arr)
    (map '(simple-array fixnum (100001)) #'1+ arr)
    (map '(simple-array fixnum (100001)) #'1+ arr)
    (map '(simple-array fixnum (100001)) #'1+ arr)
    (map '(simple-array fixnum (100001)) #'1+ arr)
    (map '(simple-array fixnum (100001)) #'1+ arr)
    (map '(simple-array fixnum (100001)) #'1+ arr)
    (map '(simple-array fixnum (100001)) #'1+ arr)
    (map '(simple-array fixnum (100001)) #'1+ arr)
    (map '(simple-array fixnum (100001)) #'1+ arr)
    (format t "map array:~%")))

(gc :full t)
(time 
  (progn 
    (map nil #'1+ arr)
    (map nil #'1+ arr)
    (map nil #'1+ arr)
    (map nil #'1+ arr)
    (map nil #'1+ arr)
    (map nil #'1+ arr)
    (map nil #'1+ arr)
    (map nil #'1+ arr)
    (map nil #'1+ arr)
    (map nil #'1+ arr)
    (map nil #'1+ arr)
    (map nil #'1+ arr)
    (map nil #'1+ arr)
    (map nil #'1+ arr)
    (map nil #'1+ arr)
    (map nil #'1+ arr)
    (map nil #'1+ arr)
    (map nil #'1+ arr)
    (map nil #'1+ arr)
    (map nil #'1+ arr)
    (format t "map nil with arrays~%")))

(format t "~%with declarations:~%~%")

(gc :full t)
(time 
  (progn
    (map 'list #'1+ (the list lis))
    (map 'list #'1+ (the list lis))
    (map 'list #'1+ (the list lis))
    (map 'list #'1+ (the list lis))
    (map 'list #'1+ (the list lis))
    (map 'list #'1+ (the list lis))
    (map 'list #'1+ (the list lis))
    (map 'list #'1+ (the list lis))
    (map 'list #'1+ (the list lis))
    (map 'list #'1+ (the list lis))
    (map 'list #'1+ (the list lis))
    (map 'list #'1+ (the list lis))
    (map 'list #'1+ (the list lis))
    (map 'list #'1+ (the list lis))
    (map 'list #'1+ (the list lis))
    (map 'list #'1+ (the list lis))
    (map 'list #'1+ (the list lis))
    (map 'list #'1+ (the list lis))
    (map 'list #'1+ (the list lis))
    (map 'list #'1+ (the list lis))
    (format t "map list:~%")))

(gc :full t)
(time 
  (progn
    (map nil #'1+ (the list lis))
    (map nil #'1+ (the list lis))
    (map nil #'1+ (the list lis))
    (map nil #'1+ (the list lis))
    (map nil #'1+ (the list lis))
    (map nil #'1+ (the list lis))
    (map nil #'1+ (the list lis))
    (map nil #'1+ (the list lis))
    (map nil #'1+ (the list lis))
    (map nil #'1+ (the list lis))
    (map nil #'1+ (the list lis))
    (map nil #'1+ (the list lis))
    (map nil #'1+ (the list lis))
    (map nil #'1+ (the list lis))
    (map nil #'1+ (the list lis))
    (map nil #'1+ (the list lis))
    (map nil #'1+ (the list lis))
    (map nil #'1+ (the list lis))
    (map nil #'1+ (the list lis))
    (map nil #'1+ (the list lis))
    (format t "map nil with lists~%")))

(gc :full t)
(time 
  (progn 
    (map 'vector #'1+ (the vector vec))
    (map 'vector #'1+ (the vector vec))
    (map 'vector #'1+ (the vector vec))
    (map 'vector #'1+ (the vector vec))
    (map 'vector #'1+ (the vector vec))
    (map 'vector #'1+ (the vector vec))
    (map 'vector #'1+ (the vector vec))
    (map 'vector #'1+ (the vector vec))
    (map 'vector #'1+ (the vector vec))
    (map 'vector #'1+ (the vector vec))
    (map 'vector #'1+ (the vector vec))
    (map 'vector #'1+ (the vector vec))
    (map 'vector #'1+ (the vector vec))
    (map 'vector #'1+ (the vector vec))
    (map 'vector #'1+ (the vector vec))
    (map 'vector #'1+ (the vector vec))
    (map 'vector #'1+ (the vector vec))
    (map 'vector #'1+ (the vector vec))
    (map 'vector #'1+ (the vector vec))
    (map 'vector #'1+ (the vector vec))
    (format t "map vector:~%")))

(gc :full t)
(time 
  (progn 
    (map nil #'1+ (the vector vec))
    (map nil #'1+ (the vector vec))
    (map nil #'1+ (the vector vec))
    (map nil #'1+ (the vector vec))
    (map nil #'1+ (the vector vec))
    (map nil #'1+ (the vector vec))
    (map nil #'1+ (the vector vec))
    (map nil #'1+ (the vector vec))
    (map nil #'1+ (the vector vec))
    (map nil #'1+ (the vector vec))
    (format t "map nil with vectors:~%")))

(gc :full t)
(time 
  (progn 
    (map '(simple-array fixnum (100001)) #'1+ (the (simple-array fixnum (100001)) arr))
    (map '(simple-array fixnum (100001)) #'1+ (the (simple-array fixnum (100001)) arr))
    (map '(simple-array fixnum (100001)) #'1+ (the (simple-array fixnum (100001)) arr))
    (map '(simple-array fixnum (100001)) #'1+ (the (simple-array fixnum (100001)) arr))
    (map '(simple-array fixnum (100001)) #'1+ (the (simple-array fixnum (100001)) arr))
    (map '(simple-array fixnum (100001)) #'1+ (the (simple-array fixnum (100001)) arr))
    (map '(simple-array fixnum (100001)) #'1+ (the (simple-array fixnum (100001)) arr))
    (map '(simple-array fixnum (100001)) #'1+ (the (simple-array fixnum (100001)) arr))
    (map '(simple-array fixnum (100001)) #'1+ (the (simple-array fixnum (100001)) arr))
    (map '(simple-array fixnum (100001)) #'1+ (the (simple-array fixnum (100001)) arr))
    (map '(simple-array fixnum (100001)) #'1+ (the (simple-array fixnum (100001)) arr))
    (map '(simple-array fixnum (100001)) #'1+ (the (simple-array fixnum (100001)) arr))
    (map '(simple-array fixnum (100001)) #'1+ (the (simple-array fixnum (100001)) arr))
    (map '(simple-array fixnum (100001)) #'1+ (the (simple-array fixnum (100001)) arr))
    (map '(simple-array fixnum (100001)) #'1+ (the (simple-array fixnum (100001)) arr))
    (map '(simple-array fixnum (100001)) #'1+ (the (simple-array fixnum (100001)) arr))
    (map '(simple-array fixnum (100001)) #'1+ (the (simple-array fixnum (100001)) arr))
    (map '(simple-array fixnum (100001)) #'1+ (the (simple-array fixnum (100001)) arr))
    (map '(simple-array fixnum (100001)) #'1+ (the (simple-array fixnum (100001)) arr))
    (map '(simple-array fixnum (100001)) #'1+ (the (simple-array fixnum (100001)) arr))
    (format t "map array:~%")))

(gc :full t)
(time 
  (progn 
    (map nil #'1+ (the (simple-array fixnum (100001)) arr))
    (map nil #'1+ (the (simple-array fixnum (100001)) arr))
    (map nil #'1+ (the (simple-array fixnum (100001)) arr))
    (map nil #'1+ (the (simple-array fixnum (100001)) arr))
    (map nil #'1+ (the (simple-array fixnum (100001)) arr))
    (map nil #'1+ (the (simple-array fixnum (100001)) arr))
    (map nil #'1+ (the (simple-array fixnum (100001)) arr))
    (map nil #'1+ (the (simple-array fixnum (100001)) arr))
    (map nil #'1+ (the (simple-array fixnum (100001)) arr))
    (map nil #'1+ (the (simple-array fixnum (100001)) arr))
    (map nil #'1+ (the (simple-array fixnum (100001)) arr))
    (map nil #'1+ (the (simple-array fixnum (100001)) arr))
    (map nil #'1+ (the (simple-array fixnum (100001)) arr))
    (map nil #'1+ (the (simple-array fixnum (100001)) arr))
    (map nil #'1+ (the (simple-array fixnum (100001)) arr))
    (map nil #'1+ (the (simple-array fixnum (100001)) arr))
    (map nil #'1+ (the (simple-array fixnum (100001)) arr))
    (map nil #'1+ (the (simple-array fixnum (100001)) arr))
    (map nil #'1+ (the (simple-array fixnum (100001)) arr))
    (map nil #'1+ (the (simple-array fixnum (100001)) arr))
    (format t "map nil with arrays~%")))