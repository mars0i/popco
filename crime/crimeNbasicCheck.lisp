;; crimeNbasicCheck.lisp

;(load "crime/crime1")
(unless (boundp 'virus-propns)
  (error 
"~%
************************************************************
************************************************************
LOAD crime propositions, person-making definitions, etc. first
************************************************************
************************************************************
~%"))

(setf *do-report-to-netlogo* nil)
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")
(SETF *DO-CONVERSE* NIL)

;(setf *propn-excit-weight* .2L0) ; traditional value from sanday sims: .2L0
;(setf *propn-inhib-weight* -.01) ; default value from sanday sims: -.025L0, i.e. 1/8 of .2

(print-parameters)

; *do-converse* is set off, so the fact that these persons are
; allowed to talk about crime will have no effect:
(make-both-bias-crime-talker 'Vicky viral-crime-propns)
(make-both-bias-crime-talker 'Becky beastly-crime-propns)
(make-both-bias-crime-talker 'Bea beast-propns)
(make-both-bias-crime-talker 'Virgil virus-propns)

;(make-both-bias-crime-talker 'nay-Vicky viral-crime-propns)
;(make-both-bias-crime-talker 'nay-Becky beastly-crime-propns)
;(make-both-bias-crime-talker 'nay-Bea beast-propns)
;(make-both-bias-crime-talker 'nay-Virgil virus-propns)

(print (get 'folks 'members))

(init-pop)

; crude low-level method to cause to *disbelieve* all formerly perceived propositions.
; note this has to happen after init-pop.
;(mapc #'negate-propn (get 'nay-Vicky-env 'all-propositions))
;(mapc #'negate-propn (get 'nay-Becky-env 'all-propositions))
;(mapc #'negate-propn (get 'nay-Bea-env 'all-propositions))
;(mapc #'negate-propn (get 'nay-Virgil-env 'all-propositions))

(setf *max-pop-ticks* 100)
;(setf *max-pop-ticks* 5000)
;(popco)
