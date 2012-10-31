;; criment6b.lisp

(load "crime/criment6")

(defvar *my-pop-size* 6)

; don't move graph around in telguess:
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")
(setf *do-converse* t)

(make-virus-bias-crime-talker 'aa crime-propns)  ; "aa" for "assured advocate" [propns are alpha-sorted, so insures this person is 1st]
(make-virus-bias-crime-talker 'temp-person)
(n-persons-with-name 'temp-person 'p (1- *my-pop-size*)) ; "p" for person
(rem-elt-from-property 'temp-person 'folks 'members)

(init-pop)
(print (get 'folks 'members))

(setf *max-pop-ticks* 1000)
(popco)
