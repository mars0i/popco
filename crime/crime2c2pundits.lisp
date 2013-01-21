;; crime2c2pundits.lisp

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
(make-beast-bias-crime-talker 'aap crime-propns) ; "aap" i.e. the positive [p] advocate, the true believer
(make-beast-bias-crime-talker 'aan crime-propns) ; "aap" i.e. the negative [n] advocate, the naysayer

;; now make the rest of the population
(make-beast-bias-crime-talker 'temp-person)
(n-persons-with-name 'temp-person 'p (- *crime-pop-size* *crime-num-pundits*)) ; "p" for person
(rem-elt-from-property 'temp-person 'folks 'members)

(init-pop)
(print (get 'folks 'members))
(popco)
(quit)
