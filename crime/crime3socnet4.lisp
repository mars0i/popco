;;;; crime3socnet4.lisp
;;;; experiment with social network functions:
;;;; Two distinct subpops who mostly don't communicate with each other, 
;;;; but have different biases, and both listen to the same pundit.
;;;; This differs from crime3socnet3.lisp in how many linking people there are.
;;;; Note that the linking people are a pretty large part of each opposing group here.

(load "nosettle")      ; don't allow networks to settle, ever, since subtle differences are amplified by communication
(load "crime/crime3")

(setf *do-report-to-netlogo* nil)

(defvar *group-size* 10)
(defvar *link-subset-size* 3)

; don't move graph around in telguess:
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")
(setf *do-converse* t)

(make-no-bias-crime-talker 'aa crime-propns 'pundits '(vulcans bajorans))  ; pundit

(make-virus-bias-crime-talker 'template-v '() 'vulcans '(vulcans))
(n-persons-with-name 'template-v 'v (- *group-size* *link-subset-size*))
(kill 'template-v)

(make-virus-bias-crime-talker 'template-vf '() '(vulcans federation) '(vulcans federation))
(n-persons-with-name 'template-vf 'vf *link-subset-size*)
(kill 'template-vf)

(make-beast-bias-crime-talker 'template-b '() 'bajorans '(bajorans))
(n-persons-with-name 'template-b 'b (- *group-size* *link-subset-size*))
(kill 'template-b)

(make-virus-bias-crime-talker 'template-bf '() '(bajorans federation) '(bajorans federation))
(n-persons-with-name 'template-bf 'bf *link-subset-size*)
(kill 'template-bf)

(init-pop)
(print (get 'folks 'members))

(setf *max-pop-ticks* 5000)
(popco)
(quit)
