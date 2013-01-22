;; crime2d2pundits.lisp

(load "nosettle")      ; don't allow networks to settle, ever, since subtle differences are amplified by communication
(load "crime/crime2")

(setf *max-pop-ticks* 5000)
(defvar *crime-pop-size* 22)
(defvar *crime-num-pundits* 2)

(setf *do-converse* t)
(setf *do-report-to-netlogo* nil)
(setf *guess-layout-commands* "") ; don't move graph around in telguess
(setf *extra-meta-commands* "")   ; ditto

;; make the pundits
;; "aa" for "assured advocate" [propns are alpha-sorted, so insures this person is 1st]:
(make-both-bias-crime-talker 'aaT crime-propns) ; "aaT" i.e. the true believer, who believes all the crime propns
(make-both-bias-crime-talker 'aaF crime-propns) ; "aaF" i.e. the the naysayer, who disbelieves all the crime propns

;; now make the rest of the population
(make-both-bias-crime-talker 'temp-person)
(n-persons-with-name 'temp-person 'p (- *crime-pop-size* *crime-num-pundits*)) ; "p" for person
(rem-elt-from-property 'temp-person 'folks 'members)

(init-pop)

; AT THIS POINT, THE PROPOSITIONS IN AAF'S ENV HAVE BEEN INITIALIZED WITH ACTIVNS = 1.0. NOW NEGATE THEM:
(mapc #'negate-propn (get 'aaF-env 'all-propositions))    ; crude low-level method to cause aa to *disbelieve* all lifestyle propositions

(print (get 'folks 'members))
(popco)
(quit)
