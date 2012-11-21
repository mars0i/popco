(load "crime/crime2")

; rename csv output file so it's easy to see what branch it's from
(defvar *gitbranch* "master")
(setf *propns-csv-output-name* (format nil "~A/~A~A.csv" *data-dir* *run-id* *gitbranch*))

;(defvar *my-pop-size* 5)

; don't move graph around in telguess:
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")

(setf *do-report-to-netlogo* nil)
(setf *do-report-analogy-nets-to-guess* nil)
(setf *do-report-propns-to-csv* t)

(setf *do-converse* NIL)

(make-no-bias-crime-talker 'nancy crime-propns)
(make-no-bias-crime-talker 'nona)
(make-beast-bias-crime-talker 'becky crime-propns)
(make-beast-bias-crime-talker 'bea)
(make-virus-bias-crime-talker 'vicky crime-propns)
(make-virus-bias-crime-talker 'vern)
(make-both-bias-crime-talker 'booker crime-propns)
(make-both-bias-crime-talker 'bob)

(init-pop)
(print (get 'folks 'members))

(setf *max-pop-ticks* 500)
;(setf *asymptote* .1L0)
(popco)
