;; crime2c.lisp

(load "crime/crime2")

(setf *do-report-to-netlogo* nil)

; rename csv output file so it's easy to see what branch it's from
;(defvar *gitbranch* (read-line (process-output (run-program "thisbranch" '() :search t :output :stream :wait nil))))
;(setf *propns-csv-output-name* (format nil "~A/~A~A.csv" *data-dir* *run-id* *gitbranch*))

(defvar *my-pop-size* 20)

; don't move graph around in telguess:
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")
(setf *do-converse* t)

; This must be called *after* init-pop
(defun half-perceived-aa (msg)
  (perceived msg .5 'aa))

(make-beast-bias-crime-talker 'aa beastly-crime-propns) ; only beastly crime propns get activn = 1

(make-beast-bias-crime-talker 'temp-person)
(n-persons-with-name 'temp-person 'p (1- *my-pop-size*)) ; "p" for person
(rem-elt-from-property 'temp-person 'folks 'members)

(init-pop)

(mapc #'half-perceived-aa viral-crime-propns)           ; viral propns get half the activn (which is too low--see lateDecSeproadback.nts--but ok it's an experiment)

(print (get 'folks 'members))

(setf *max-pop-ticks* 1)
;(popco)
