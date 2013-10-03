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

(defun set-slots (s)
  (setf (mystruct-p2 s) 19.1)
  (setf (mystruct-p3 s) 19.1)
  (setf (mystruct-p4 s) 19.1)
  (setf (mystruct-p5 s) 19.1)
  (setf (mystruct-p6 s) 19.1)
  (setf (mystruct-p7 s) 19.1)
  (setf (mystruct-p8 s) 19.1)
  (setf (mystruct-p9 s) 19.1)
  (setf (mystruct-p10 s) 19.1)
  (setf (mystruct-p11 s) 19.1)
  (setf (mystruct-p12 s) 19.1)
  (setf (mystruct-p13 s) 19.1)
  (setf (mystruct-p14 s) 19.1)
  (setf (mystruct-p15 s) 19.1)
  (setf (mystruct-p16 s) 19.1)
  (setf (mystruct-p17 s) 19.1)
  (setf (mystruct-p18 s) 19.1)
  (setf (mystruct-p19 s) 19.1)
  (setf (mystruct-p20 s) 19.1)
  (setf (mystruct-p1 s) 19.1))

(defun set-props (s)
  (setf (get s 'p2) 19.1)
  (setf (get s 'p3) 19.1)
  (setf (get s 'p4) 19.1)
  (setf (get s 'p5) 19.1)
  (setf (get s 'p6) 19.1)
  (setf (get s 'p7) 19.1)
  (setf (get s 'p8) 19.1)
  (setf (get s 'p9) 19.1)
  (setf (get s 'p10) 19.1)
  (setf (get s 'p11) 19.1)
  (setf (get s 'p12) 19.1)
  (setf (get s 'p13) 19.1)
  (setf (get s 'p14) 19.1)
  (setf (get s 'p15) 19.1)
  (setf (get s 'p16) 19.1)
  (setf (get s 'p17) 19.1)
  (setf (get s 'p18) 19.1)
  (setf (get s 'p19) 19.1)
  (setf (get s 'p20) 19.1)
  (setf (get s 'p1) 19.1))

(defparameter times 1000000)

(format t "struct slots:~%")
(time (dotimes (ignored times)
        (set-slots structvar)))

(format t "prop list:~%")
(time (dotimes (ignored times)
        (set-props 'propsvar)))
