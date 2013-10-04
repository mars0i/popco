(defstruct mystruct
  p1
  p2
  p3
  p4
  p5
  p6
  p7
  p8
  p9
  p10
  p11
  p12
  p13
  p14
  p15
  p16
  p17
  p18
  p19
  p20)

(defvar structvar (make-mystruct 
  :p1 1
  :p2 2
  :p3 3
  :p4 4
  :p5 5
  :p6 6
  :p7 7
  :p8 8
  :p9 9
  :p10 10
  :p11 11
  :p12 12
  :p13 13
  :p14 14
  :p15 15
  :p16 16
  :p17 17
  :p18 18
  :p19 19
  :p20 20))

(defvar propsvar 't)
(setf (get 'propsvar 'p1) 1)
(setf (get 'propsvar 'p2) 2)
(setf (get 'propsvar 'p3) 3)
(setf (get 'propsvar 'p4) 4)
(setf (get 'propsvar 'p5) 5)
(setf (get 'propsvar 'p6) 6)
(setf (get 'propsvar 'p7) 7)
(setf (get 'propsvar 'p8) 8)
(setf (get 'propsvar 'p9) 9)
(setf (get 'propsvar 'p10) 10)
(setf (get 'propsvar 'p11) 11)
(setf (get 'propsvar 'p12) 12)
(setf (get 'propsvar 'p13) 13)
(setf (get 'propsvar 'p14) 14)
(setf (get 'propsvar 'p15) 15)
(setf (get 'propsvar 'p16) 16)
(setf (get 'propsvar 'p17) 17)
(setf (get 'propsvar 'p18) 18)
(setf (get 'propsvar 'p19) 19)
(setf (get 'propsvar 'p20) 20)

(defun rot-slots (s)
  (make-mystruct
    :p1 (mystruct-p20 s)
    :p2 (mystruct-p1 s)
    :p3 (mystruct-p2 s)
    :p4 (mystruct-p3 s)
    :p5 (mystruct-p4 s) 
    :p6 (mystruct-p5 s)  
    :p7 (mystruct-p6 s)   
    :p8 (mystruct-p7 s)    
    :p9 (mystruct-p8 s)     
    :p10 (mystruct-p9 s)     
    :p11 (mystruct-p10 s)
    :p12 (mystruct-p11 s)
    :p13 (mystruct-p12 s)
    :p14 (mystruct-p13 s)
    :p15 (mystruct-p14 s)
    :p16 (mystruct-p15 s)
    :p17 (mystruct-p16 s)
    :p18 (mystruct-p17 s)
    :p19 (mystruct-p18 s)
    :p20 (mystruct-p19 s)))

(defparameter times 100000)

(defun mytest ()
  ;(gc)
  (time (dotimes (ignored times)
          (rot-slots structvar))))

(format t "run (mytest)~%")
