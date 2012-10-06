;; parenting18analogsAsBiases2e.lisp

(load "sanday/parenting18analogsAsBiases2")

(defvar *my-pop-size* 10)

; don't move graph around in telguess:
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")
(setf *do-converse* t)

(make-sky-biased-lifestyle-talker 'aa lifestyle-propns)  ; "aa" for "assured advocate" [propns are alpha-sorted, so insures this person is 1st]
(make-sky-biased-lifestyle-talker 'temp-person)
(n-persons-with-name 'temp-person 'p (1- *my-pop-size*)) ; "p" for person
(rem-elt-from-property 'temp-person 'folks 'members)

(init-pop)
(print (get 'folks 'members))

(setf *max-pop-ticks* 1500)
(popco)
