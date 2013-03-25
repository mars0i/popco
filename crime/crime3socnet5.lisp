;;;; crime3socnet5.lisp
;;;; Experiment with social network functions.
;;;; Two distinct subpops who mostly don't communicate with each other, 
;;;; but have different biases, and both listen to the same pundit.
;;;;
;;;; THIS MODEL DIFFERS FROM crime3socnet4.lisp BY USING LARGER SUBPOPS,
;;;; and a larger population overall.  However, I kept the proportion
;;;; of population members willing to talk to members of the other
;;;; population the same.  This doesn't clearly make the effect of
;;;; between-group communication equivalent to that in the smaller
;;;; model, since variance effects might matter.

(load "nosettle")      ; don't allow networks to settle, ever, since subtle differences are amplified by communication
(load "crime/crime3")

(setf *do-report-to-netlogo* nil)

(defvar *group-size* 20)
(defvar *link-subset-size* 6)

; don't move graph around in telguess:
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")
(setf *do-converse* t)

;;; see comment below for meaning of parameters

;; The Pundit (the environment, the TV, etc.)
(make-no-bias-crime-talker 'aa crime-propns 'pundits '(vulcans bajorans))  ; pundit

;; Vulcans who talk only to to Vulcans
(make-virus-bias-crime-talker 'template-v '() 'vulcans '(vulcans))
(n-persons-with-name 'template-v 'v (- *group-size* *link-subset-size*))
(kill 'template-v)

;; Vulcans who talk to friendly Bajorans
;;                              my name    I see      my groups           I talk to
(make-virus-bias-crime-talker 'template-vf '() '(vulcans federation) '(vulcans federation))
(n-persons-with-name 'template-vf 'vf *link-subset-size*)
(kill 'template-vf)

;; Bajorans who talk only to Bajorans
(make-beast-bias-crime-talker 'template-b '() 'bajorans '(bajorans))
(n-persons-with-name 'template-b 'b (- *group-size* *link-subset-size*))
(kill 'template-b)

;; Bajorans who talk to friendly Vulcans
(make-virus-bias-crime-talker 'template-bf '() '(bajorans federation) '(bajorans federation))
(n-persons-with-name 'template-bf 'bf *link-subset-size*)
(kill 'template-bf)

(init-pop)
(print (get 'folks 'members))

(setf *max-pop-ticks* 5000)
(popco)
(quit)
