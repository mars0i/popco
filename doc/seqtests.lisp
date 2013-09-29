
;; non-tail recursive version:
;(defun ints (start end)
;  (if (>= start end)
;    nil
;    (cons start (ints (1+ start) end))))


;; tail recursive version (SBCL is tail recursive)

(defun ints-aux (start end acc)
  (if (>= start end)
    acc
    (ints-aux start (1- end) (cons (1- end) acc))))

(defun ints (start end)
  (ints-aux start end (list end)))

(defparameter lis1 (ints 0 100000))
(defparameter lis2 (ints 3 100003))
(defparameter vec1 (apply #'vector lis1))
(defparameter vec2 (apply #'vector lis2))

;(defconstant lis1 (ints 0 100000))
;(defconstant lis2 (ints 3 100003))
;(defconstant vec1 (apply #'vector lis1))
;(defconstant vec2 (apply #'vector lis2))

(format t "done initializing~%~%")

(format t "~%without declarations:~%~%")

(gc :full t)
(time 
  (progn
    (dotimes (ignored 10)
      (mapcar #'+ lis1 lis2))
    (format t "mapcar:~%")))

(gc :full t)
(time 
  (progn
    (dotimes (ignored 10)
      (mapc #'+ lis1 lis2))
    (format t "mapc:~%")))

(gc :full t)
(time 
  (progn
    (dotimes (ignored 10)
      (map 'list #'+ lis1 lis2))
    (format t "map list:~%")))

(gc :full t)
(time 
  (progn
    (dotimes (ignored 10)
      (map nil #'+ lis1 lis2))
    (format t "map nil with lists~%")))

(gc :full t)
(time 
  (progn 
    (dotimes (ignored 10)
      (map 'vector #'+ vec1 vec2))
    (format t "map vector:~%")))

(gc :full t)
(time 
  (progn 
    (dotimes (ignored 10)
      (map nil #'+ vec1 vec2))
    (format t "map nil with vectors:~%")))

(format t "~%with declarations:~%~%")

(gc :full t)
(time 
  (progn
    (dotimes (ignored 10)
      (map 'list #'+ (the list lis1) (the list lis2)))
    (format t "map list:~%")))

(gc :full t)
(time 
  (progn
    (dotimes (ignored 10)
      (map nil #'+ (the list lis1) (the list lis2)))
    (format t "map nil with lists~%")))

(gc :full t)
(time 
  (progn 
    (dotimes (ignored 10)
      (map 'vector #'+ (the vector vec1) (the vector vec2)))
    (format t "map vector:~%")))

(gc :full t)
(time 
  (progn 
    (dotimes (ignored 10)
      (map nil #'+ (the vector vec1) (the vector vec2)))
    (format t "map nil with vectors:~%")))
