(load "crime/crime2")

; rename csv output file so it's easy to see what branch it's from
(defvar *gitbranch* 
  (read-line (process-output (run-program "thisbranch" '() :search t :output :stream :wait nil))))
  ; Yes--this is what you have to do to run an external program and get its stdout using SBCL's built in unix support functions!
  ; Just doing our part to popularize Lisp with the doubters ....

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

(setf *max-pop-ticks* 100)
;(setf *asymptote* .1L0)
(popco)

(format t "git branch = ~S~%" *gitbranch*)

(quit)
