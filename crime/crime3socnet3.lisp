;;;; crime3socnet3.lisp
;;;; experiment with social network functions:
;;;; Two distinct subpops who mostly don't communicate with each other, 
;;;; but have different biases, and both listen to the same pundit.
;;;; This differs from crime3socnet1.lisp in that there is a pair of
;;;; individuals, one from each subpop, who are willing to talk to each other.
;;;; They're given distinguished names, "whorf" [sic--s/b "worf"] and "kira".
;;;; [Note that since by default each person only talks to one
;;;; individual on each tick, Whorf and Kira will only talk to each other
;;;; about 1/10 of the time, other things being equal.]

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
;; Oops, should have been 'worf'.  
;; (Then again, why not merge Benjamin Lee and the son of Mogh?)

(make-beast-bias-crime-talker 'template-b '() 'bajorans '(bajorans))
(n-persons-with-name 'template-b 'b (1- *group-size*))
(kill 'template-b)
(make-beast-bias-crime-talker 'kira '() '(bajorans ds9) '(bajorans ds9))

(init-pop)
(print (get 'folks 'members))

(setf *max-pop-ticks* 5000)
(popco)
(quit)
