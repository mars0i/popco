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
  (setf (mystruct-p2 s) (mystruct-p1 s))
  (setf (mystruct-p3 s) (mystruct-p2 s))
  (setf (mystruct-p4 s) (mystruct-p3 s))
  (setf (mystruct-p5 s) (mystruct-p4 s))
  (setf (mystruct-p6 s) (mystruct-p5 s))
  (setf (mystruct-p7 s) (mystruct-p6 s))
  (setf (mystruct-p8 s) (mystruct-p7 s))
  (setf (mystruct-p9 s) (mystruct-p8 s))
  (setf (mystruct-p10 s) (mystruct-p9 s))
  (setf (mystruct-p11 s) (mystruct-p10 s))
  (setf (mystruct-p12 s) (mystruct-p11 s))
  (setf (mystruct-p13 s) (mystruct-p12 s))
  (setf (mystruct-p14 s) (mystruct-p13 s))
  (setf (mystruct-p15 s) (mystruct-p14 s))
  (setf (mystruct-p16 s) (mystruct-p15 s))
  (setf (mystruct-p17 s) (mystruct-p16 s))
  (setf (mystruct-p18 s) (mystruct-p17 s))
  (setf (mystruct-p19 s) (mystruct-p18 s))
  (setf (mystruct-p20 s) (mystruct-p19 s))
  (setf (mystruct-p1 s) (mystruct-p20 s)))

(defun rot-props (s)
  (setf (get s 'p2) (get s 'p1))
  (setf (get s 'p3) (get s 'p2))
  (setf (get s 'p4) (get s 'p3))
  (setf (get s 'p5) (get s 'p4))
  (setf (get s 'p6) (get s 'p5))
  (setf (get s 'p7) (get s 'p6))
  (setf (get s 'p8) (get s 'p7))
  (setf (get s 'p9) (get s 'p8))
  (setf (get s 'p10) (get s 'p9))
  (setf (get s 'p11) (get s 'p10))
  (setf (get s 'p12) (get s 'p11))
  (setf (get s 'p13) (get s 'p12))
  (setf (get s 'p14) (get s 'p13))
  (setf (get s 'p15) (get s 'p14))
  (setf (get s 'p16) (get s 'p15))
  (setf (get s 'p17) (get s 'p16))
  (setf (get s 'p18) (get s 'p17))
  (setf (get s 'p19) (get s 'p18))
  (setf (get s 'p20) (get s 'p19))
  (setf (get s 'p1) (get s 'p20)))

(defparameter times 1000000)

(format t "struct slots:~%")
(time (dotimes (ignored times)
        (rot-slots structvar)))

(format t "prop list:~%")
(time (dotimes (ignored times)
        (rot-props propsvar)))
