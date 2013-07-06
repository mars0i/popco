;;;; dispersalNoTalk.lisp
;;;; For possible use with papers/Dispersal

;;; Show no-conversation differences resulting from having different
;;; source analogs when experiencing two different kinds of crime perception.

;;; Especially: What is the difference between having both source analogs
;;; (virus, beast) and having only one when you are faced with an environment
;;; in which there is one of {beastly-crime, viral-crime}.
;;; Hypothesis: Your response is sensible if and only iff you have available
;;; the appropriate source analog.

;;; Tip: Run once to produce a csv file, then process that with read2multirunRA to
;;; produce an mra (multi-run array) object in R.  Then run activnsAtTickBarchart
;;; on the mra, e.g. thus: activnsAtTickBarchart(dispersalNoTalk.mra, 300)  .

(load "nosettle")      ; don't allow networks to settle, ever, since subtle differences are amplified by communication
(load "crime/crime3")

(setf *do-report-to-netlogo* nil)

;(defvar *group-size* 20)
;(defvar *link-subset-size* 6)

; don't move graph around in telguess:
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")

(setf *DO-CONVERSE* NIL)

;;; see comment below for meaning of parameters

(make-both-bias-crime-talker  'bobe beastly-crime-propns)  ; expect coherent response
(make-virus-bias-crime-talker 'vibe beastly-crime-propns)  ; expect INCOHERENT response
(make-beast-bias-crime-talker 'bebe beastly-crime-propns)  ; expect coherent response

(make-both-bias-crime-talker  'bovi viral-crime-propns)    ; expect coherent response
(make-virus-bias-crime-talker 'vivi viral-crime-propns)    ; expect coherent response
(make-beast-bias-crime-talker 'bevi viral-crime-propns)    ; expect INCOHERENT response

(init-pop)
(print (get 'folks 'members))

(setf *max-pop-ticks* 300)
(popco)
;(quit)
