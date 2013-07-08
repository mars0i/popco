;;;; dispersal1.lisp
;;;; (similar to crime3socnet1.lisp)
;;;; four distinct subpops who don't communicate with each other, 
;;;; but have different biases, and listen to the same pundit.

(load "nosettle")      ; don't allow networks to settle, ever, since subtle differences are amplified by communication
(load "crime/crime3")

(setf *do-report-to-netlogo* nil)

(defvar *group-size* 40)

; don't move graph around in telguess:
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")
(setf *do-converse* t)

(make-no-bias-crime-talker 'aa crime-propns 'pundits '(VIBIs BEBIs BOBIs NOBIs))  ; pundit

(make-virus-bias-crime-talker 'temp-vi '() 'vibis '(vibis))
(n-persons-with-name 'temp-v 'vi *group-size*)
(kill 'temp-v)

(make-beast-bias-crime-talker 'temp-be '() 'bebis '(bebis))
(n-persons-with-name 'temp-b 'be *group-size*)
(kill 'temp-b)

(make-both-bias-crime-talker 'temp-bo '() 'bobis '(bobis))
(n-persons-with-name 'temp-b 'bo *group-size*)
(kill 'temp-b)

(make-no-bias-crime-talker 'temp-no '() 'nobis '(nobis))
(n-persons-with-name 'temp-b 'no *group-size*)
(kill 'temp-b)

(init-pop)
(print (get 'folks 'members))

(setf *max-pop-ticks* 5000)
(popco)
(quit)
