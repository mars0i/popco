;; crime3b.lisp

(load "nosettle")      ; don't allow networks to settle, ever, since subtle differences are amplified by communication
(load "crime/crime3")

(setf *do-report-to-netlogo* nil)

(defvar *group-size* 10)

; don't move graph around in telguess:
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")
(setf *do-converse* t)

(make-no-bias-crime-talker 'aa crime-propns 'pundits '(venusians betazoids))  ; pundit

(make-virus-bias-crime-talker 'temp-v '() 'venusians '(venusians))
(n-persons-with-name 'temp-person 'v *group-size*)

(make-beast-bias-crime-talker 'temp-b '() 'betazoids '(betazoids))
(n-persons-with-name 'temp-person 'b *group-size*)

(init-pop)
;(rem-elt-from-property 'temp-v 'folks 'members) ; has to happen after init-pop when groups are merged into 'folks
;(rem-elt-from-property 'temp-b 'folks 'members)
(print (get 'folks 'members))

(setf *max-pop-ticks* 500)
;(popco)
;(quit)
