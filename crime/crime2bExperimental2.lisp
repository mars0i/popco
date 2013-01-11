;; crime2bExperimental2.lisp

(load "crime/crime2")

(defvar *my-pop-size* 21)

; don't move graph around in telguess:
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")
(setf *do-report-to-netlogo* nil)
(setf *do-converse* t)

(make-virus-bias-crime-talker 'av viral-crime-propns)
(make-virus-bias-crime-talker 'ab1 beastly-crime-propns)
(make-virus-bias-crime-talker 'ab2 beastly-crime-propns)
(make-virus-bias-crime-talker 'temp-person)
(n-persons-with-name 'temp-person 'p (1- *my-pop-size*)) ; "p" for person
(rem-elt-from-property 'temp-person 'folks 'members)

(init-pop)
(print (get 'folks 'members))

(setf *max-pop-ticks* 5000)
(popco)
(quit)
