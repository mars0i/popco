;; parenting18analogsAsBiases2g.lisp

(load "sanday/parenting18analogsAsBiases2")

(defvar *number-of-followers* 10)

; don't move graph around in telguess:
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")
(setf *do-converse* t)

(make-all-origin-lifestyle-talker 'aaF lifestyle-propns)  ; "aa" for "assured advocate": NAYSAYER      [propns are alpha-sorted, so will sort before followers]
(make-all-origin-lifestyle-talker 'aaT lifestyle-propns)  ; "aa" for "assured advocate": TRUE BELIEVER [propns are alpha-sorted, so will sort before followers]
(make-all-origin-lifestyle-talker 'temp-person)
(n-persons-with-name 'temp-person 'p (1- *number-of-followers*)) ; "p" for person
(rem-elt-from-property 'temp-person 'folks 'members)

(init-pop)

; AT THIS POINT, THE PROPOSITIONS IN AAF'S ENV HAVE BEEN INITIALIZED WITH ACTIVNS = 1.0. NOW NEGATE THEM:
(mapc #'negate-propn (get 'aaf-env 'all-propositions))    ; crude low-level method to cause aa to *disbelieve* all lifestyle propositions

(print (get 'folks 'members))

(setf *max-pop-ticks* 1500)
(popco)
