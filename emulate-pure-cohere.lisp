; emulate-pure-cohere.lisp
; Settings to cause POPCO to emulate a pre-POPCO, pure Thagard COHERE
; system in POPCO.  More specifically, these settings are supposed to
; make each person into a discrete old-style COHERE system.

; Currently only designed to emulate pure ACME: 
; - Proposition-net creation and updating are disabled.
; - Conversations are disabled.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; POPCO settings to make each POPCO person function like an entire ACME system:

(setf *do-converse* nil)
(setf *do-update-propn-nets* nil)
(setf *do-report-to-netlogo* nil)
(setf *max-times* 1) ; one cycle per pop-tick
(setf *max-pop-ticks* 50)
(setf *sleep-delay* nil)
(setf *pop-tick-label* "cycle") ; What each pop-tick is described as in Guess
