;; crime1basicCheck1.lisp

(load "crime/crime2")

; don't move graph around in telguess:
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")
(setf *do-converse* NIL)
(setf *do-update-propn-nets* NIL)
(setf *do-report-to-netlogo* nil)
(setf *do-report-propns-to-csv* nil)
(setf *do-report-analogy-nets-to-guess* t)

;(setf *propn-excit-weight* .2L0) ; traditional value from sanday sims: .2L0
;(setf *propn-inhib-weight* -.01) ; default value from sanday sims: -.025L0, i.e. 1/8 of .2

(print-parameters)

(make-both-bias-crime-talker 'Vicky viral-crime-propns)
(make-both-bias-crime-talker 'Becky beastly-crime-propns)

(print (get 'folks 'members))

(init-pop)

(setf *max-pop-ticks* 40)
;(popco)
