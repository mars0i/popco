;;;; dispersalNoTalkMissingBeasts.lisp
;;;; For possible use with papers/Dispersal

;;; Show no-conversation differences resulting from having different
;;; source analogs when experiencing two different kinds of crime perception.

;;; Specifically, what happens when you remove individual target analogs from
;;; perception of BEASTLY-CRIME?  Can they get restored, ever?  Like what analogies are supposed to be
;;; able to do, even though I haven't explicitly got an implementation of that
;;; functionality?

;;; Especially: What is the difference between having both source analogs
;;; (virus, beast) and having only one when you are faced with an environment
;;; in which there is one of {beastly-crime, viral-crime}.
;;; What would be a nice result: Your response is sensible in the sense of filling 
;;; in what's missing if and only iff you have available the appropriate source analog.

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

(make-both-bias-crime-talker  'bobe0 (rem-nth 0 beastly-crime-propns))  ; expect coherent response
(make-both-bias-crime-talker  'bobe1 (rem-nth 1 beastly-crime-propns))  ; expect coherent response
(make-both-bias-crime-talker  'bobe2 (rem-nth 2 beastly-crime-propns))  ; expect coherent response
(make-both-bias-crime-talker  'bobe3 (rem-nth 3 beastly-crime-propns))  ; expect coherent response
(make-both-bias-crime-talker  'bobe4 (rem-nth 4 beastly-crime-propns))  ; expect coherent response
(make-both-bias-crime-talker  'bobe5 (rem-nth 5 beastly-crime-propns))  ; expect coherent response
(make-both-bias-crime-talker  'bobe6 (rem-nth 6 beastly-crime-propns))  ; expect coherent response
(make-both-bias-crime-talker  'bobe7 (rem-nth 7 beastly-crime-propns))  ; expect coherent response
(make-both-bias-crime-talker  'bobe8 (rem-nth 8 beastly-crime-propns))  ; expect coherent response
(make-both-bias-crime-talker  'bobe9 (rem-nth 9 beastly-crime-propns))  ; expect coherent response
(make-both-bias-crime-talker  'bobe10 (rem-nth 10 beastly-crime-propns))  ; expect coherent response
(make-both-bias-crime-talker  'bobe11 (rem-nth 11 beastly-crime-propns))  ; expect coherent response

(make-virus-bias-crime-talker 'vibe0 (rem-nth 0 beastly-crime-propns))  ; expect INCOHERENT response
(make-virus-bias-crime-talker 'vibe1 (rem-nth 1 beastly-crime-propns))  ; expect INCOHERENT response
(make-virus-bias-crime-talker 'vibe2 (rem-nth 2 beastly-crime-propns))  ; expect INCOHERENT response
(make-virus-bias-crime-talker 'vibe3 (rem-nth 3 beastly-crime-propns))  ; expect INCOHERENT response
(make-virus-bias-crime-talker 'vibe4 (rem-nth 4 beastly-crime-propns))  ; expect INCOHERENT response
(make-virus-bias-crime-talker 'vibe5 (rem-nth 5 beastly-crime-propns))  ; expect INCOHERENT response
(make-virus-bias-crime-talker 'vibe6 (rem-nth 6 beastly-crime-propns))  ; expect INCOHERENT response
(make-virus-bias-crime-talker 'vibe7 (rem-nth 7 beastly-crime-propns))  ; expect INCOHERENT response
(make-virus-bias-crime-talker 'vibe8 (rem-nth 8 beastly-crime-propns))  ; expect INCOHERENT response
(make-virus-bias-crime-talker 'vibe9 (rem-nth 9 beastly-crime-propns))  ; expect INCOHERENT response
(make-virus-bias-crime-talker 'vibe10 (rem-nth 10 beastly-crime-propns))  ; expect INCOHERENT response
(make-virus-bias-crime-talker 'vibe11 (rem-nth 11 beastly-crime-propns))  ; expect INCOHERENT response

;(make-beast-bias-crime-talker 'bebe beastly-crime-propns)  ; expect coherent response

;(make-both-bias-crime-talker  'bovi viral-crime-propns)    ; expect coherent response
;(make-virus-bias-crime-talker 'vivi viral-crime-propns)    ; expect coherent response
;(make-beast-bias-crime-talker 'bevi viral-crime-propns)    ; expect INCOHERENT response

(init-pop)
(print (get 'folks 'members))

(setf *max-pop-ticks* 300)
(popco)
(quit)

