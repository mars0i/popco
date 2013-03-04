;;;; crime3socnet2.lisp
;;;; Experiment related to social network functions:
;;;; One population with full random communication, but with
;;;; two subcultures which differ in whether they have virus or 
;;;; beast analogs.  This is the opposite extreme from crime3socnet1.lisp,
;;;; which essentially duplicated separate runs with different biases.
;;;; [Next step will be to allow the two subpops to be linked by only
;;;; one or a few communication link.]

(load "nosettle")      ; don't allow networks to settle, ever, since subtle differences are amplified by communication
(load "crime/crime3")

(setf *do-report-to-netlogo* nil)

(defvar *group-size* 10)

; don't move graph around in telguess:
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")
(setf *do-converse* t)

(make-no-bias-crime-talker 'aa crime-propns 'pundits '(vulcans betazoids))  ; pundit

(make-virus-bias-crime-talker 'template-v '() 'vulcans '(vulcans betazoids))
(n-persons-with-name 'template-v 'v *group-size*)
(kill 'template-v)

(make-beast-bias-crime-talker 'template-b '() 'betazoids '(vulcans betazoids))
(n-persons-with-name 'template-b 'b *group-size*)
(kill 'template-b)

(init-pop)
(print (get 'folks 'members))

(setf *max-pop-ticks* 5000)
(popco)
(quit)
