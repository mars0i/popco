;; parenting18analogsAsBiases2a.lisp

(load "sanday/parenting18analogsAsBiases2")

(defvar *my-pop-size* 10)

; don't move graph around in telguess:
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")
(setf *do-converse* t)

(make-all-origin-lifestyle-talker 'temp-person)
(n-persons-with-name 'temp-person 's (1- *my-pop-size*)) ; "s" for sky-based
(rem-elt-from-property 'temp-person 'folks 'members)
(make-all-origin-lifestyle-talker 'sp lifestyle-propns) ; "sp" for sky-based perceiver

(init-pop)
(print (get 'folks 'members))

(setf *max-pop-ticks* 1500)
(popco)
