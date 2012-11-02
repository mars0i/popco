;; criment6basicCheck1.lisp

(load "crime/criment6")

; don't move graph around in telguess:
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")
(setf *do-converse* NIL)

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

(setf *max-pop-ticks* 50)
(popco)
