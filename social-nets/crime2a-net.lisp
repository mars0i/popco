;; crime2a-net.lisp
;; taken directly from crime/crime2a, without some of the extra stuff
;; trying to figure out what's different between my choose-conversers and
;; Dr. Abrams' that makes this model not work...
(load "networking/networking2-functions.lisp")
(load "crime/crime2")


(SETF *DO-REPORT-TO-NETLOGO* t)
(defvar *my-pop-size* 20)
(setf *do-converse* t)

(make-no-bias-crime-talker 'aa crime-propns)
(make-no-bias-crime-talker 'temp-person)
(n-persons-with-name 'temp-person 'p (1- *my-pop-size*)) ; "p" for person
(rem-elt-from-property 'temp-person 'folks 'members)

(init-pop)
(print (get 'folks 'members))

(setf *max-pop-ticks* 50)
(popco)