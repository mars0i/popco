;;;; crime3socnet1.lisp
;;;; experiment with social network functions
;;;; two distinct subpops who don't communicate with each other, 
;;;; but have different biases, and both listen to the same pundit.
;;;; This should roughly duplicate the effect of running 
;;;; crime3b.lisp and crime3c.lisp separately, although initially
;;;; I'm doing these runs with fewer members in each subpop (10) rather
;;;; than the 20 + pundit in crime3b.lisp and crime3c.lisp.

(load "nosettle")      ; don't allow networks to settle, ever, since subtle differences are amplified by communication
(load "crime/crime3")

(setf *do-report-to-netlogo* nil)

(defvar *group-size* 10)

; don't move graph around in telguess:
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")
(setf *do-converse* t)

(make-no-bias-crime-talker 'aa crime-propns 'pundits '(vulcans betazoids))  ; pundit

(make-virus-bias-crime-talker 'temp-v '() 'vulcans '(vulcans))
(n-persons-with-name 'temp-v 'v *group-size*)
(kill 'temp-v)

(make-beast-bias-crime-talker 'temp-b '() 'betazoids '(betazoids))
(n-persons-with-name 'temp-b 'b *group-size*)
(kill 'temp-b)

(init-pop)
(print (get 'folks 'members))

(setf *max-pop-ticks* 500)
;(popco)
;(quit)
