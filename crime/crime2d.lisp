;; crime2d.lisp

(load "crime/crime2")

(SETF *DO-REPORT-TO-NETLOGO* NIL)

; rename csv output file so it's easy to see what branch it's from
(defvar *gitbranch* (read-line (process-output (run-program "thisbranch" '() :search t :output :stream :wait nil))))
(setf *propns-csv-output-name* (format nil "~A/~A~A.csv" *data-dir* *run-id* *gitbranch*))

(defvar *my-pop-size* 20)

; don't move graph around in telguess:
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")
(setf *do-converse* t)

(make-both-bias-crime-talker 'aa crime-propns)  ; "aa" for "assured advocate" [propns are alpha-sorted, so insures this person is 1st]
(make-both-bias-crime-talker 'temp-person)
(n-persons-with-name 'temp-person 'p (1- *my-pop-size*)) ; "p" for person
(rem-elt-from-property 'temp-person 'folks 'members)

(init-pop)
(print (get 'folks 'members))

(setf *max-pop-ticks* 5000)
(popco)
(format t "git branch = ~S.  csv file = ~S.  *min-pop-ticks-to-settle* = ~S~%" *gitbranch* *propns-csv-output-name* *min-pop-ticks-to-settle*)
(quit)
