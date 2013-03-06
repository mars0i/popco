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

(make-no-bias-crime-talker 'aa crime-propns 'pundits '(vulcans bajorans))  ; pundit

(make-virus-bias-crime-talker 'template-v '() 'vulcans '(vulcans))
(n-persons-with-name 'template-v 'v (1- *group-size*))
(kill 'template-v)
(make-virus-bias-crime-talker 'whorf '() '(vulcans ds9) '(vulcans ds9))

(make-beast-bias-crime-talker 'template-b '() 'bajorans '(bajorans))
(n-persons-with-name 'template-b 'b (1- *group-size*))
(kill 'template-b)
(make-beast-bias-crime-talker 'kira '() '(bajorans ds9) '(bajorans ds9))

(init-pop)
(print (get 'folks 'members))

(setf *max-pop-ticks* 7500)
(popco)
(quit)
