;; parenting18analogsAsBiases2h.lisp

(load "sanday/parenting18analogsAsBiases2")

; 11 instead of 10: There will be two true believers, leaving 9 followers, just as in parenting18analogsAsBiases2[a-f].lisp:
(defvar *my-pop-size* 11)

; don't move graph around in telguess:
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")
(setf *do-converse* t)

(make-sky-biased-lifestyle-talker 'aaF lifestyle-propns)  ; "aa" for "assured advocate": NAYSAYER      [propns are alpha-sorted, so will sort before followers]
(make-sky-biased-lifestyle-talker 'aaT lifestyle-propns)  ; "aa" for "assured advocate": TRUE BELIEVER [propns are alpha-sorted, so will sort before followers]
(make-sky-biased-lifestyle-talker 'temp-person)
(n-persons-with-name 'temp-person 'p (1- *my-pop-size*)) ; "p" for person
(rem-elt-from-property 'temp-person 'folks 'members)

(init-pop)

; AT THIS POINT, THE PROPOSITIONS IN AAF'S ENV HAVE BEEN INITIALIZED WITH ACTIVNS = 1.0. NOW NEGATE THEM:
(mapc #'negate-propn (get 'aaf-env 'all-propositions))    ; crude low-level method to cause aa to *disbelieve* all lifestyle propositions

(print (get 'folks 'members))

(setf *max-pop-ticks* 1500)
(popco)
