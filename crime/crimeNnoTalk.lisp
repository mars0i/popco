;;; crime2notalk.lisp
;;; Shows what happens to crime beliefs with crime2.lisp setup 
;;; when there is no conversation.  i.e. make everyone a true-believer,
;;; differing only in the virus/beast analogies available.

;; NOTE this test differs from crimeNbasicCheck.lisp.
;; crimeNbasicCheck.lisp: Everyone has all analogs; beliefs perceived vary.  
;; crimeNnoTalk.lisp: Everyone perceives all crime propositions; analogs vary.

;(load "crime/crime2")
(unless (boundp 'crime-propns)
  (error 
"~%
************************************************************
************************************************************
LOAD crime propositions, person-making definitions, etc. first
************************************************************
************************************************************
~%"))

(setf *do-report-to-netlogo* nil)
;(setf *guess-layout-commands* "")
;(setf *extra-meta-commands* "")
(SETF *DO-CONVERSE* NIL)

; *do-converse* is set off, so the fact that these persons are
; allowed to talk about crime will have no effect:
(make-no-bias-crime-talker 'nona crime-propns)
(make-virus-bias-crime-talker 'virgil crime-propns)
(make-beast-bias-crime-talker 'betsy crime-propns)
(make-both-bias-crime-talker 'beavis crime-propns)

(init-pop)
(print-parameters)
(print (get 'folks 'members))

;(setf *max-pop-ticks* 100)
;(setf *max-pop-ticks* 5000)
(setf *max-pop-ticks* 1)
;(popco)
;(quit)
