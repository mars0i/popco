;; crime2CompareSemanticIffs.lisp
;; use to compare crime2 persons with and without semantic-iffs

(load "crime/crime2")

; special defs for persons with special semantic limited specs
(defvar just-similar 
  '((similar 'cause 'prevent (* -1 *ident-weight*))))

(defun make-no-iff-no-bias-crime-talker (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start (,@crime-propns)))
                   (make-struc 'source 'problem '(start ()))
                   ,@just-similar)
                 '()
                 '(target)))

(defun make-no-iff-virus-bias-crime-talker (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start (,@crime-propns)))
                   (make-struc 'source 'problem '(start (,@virus-propns)))
                   ,@just-similar)
                 '()
                 '(target)))

(defun make-no-iff-beast-bias-crime-talker (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start (,@crime-propns)))
                   (make-struc 'source 'problem '(start (,@beast-propns)))
                   ,@just-similar)
                 '()
                 '(target)))

(defun make-no-iff-both-bias-crime-talker (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start (,@crime-propns)))
                   (make-struc 'source 'problem '(start (,@virus-propns ,@beast-propns)))
                   ,@just-similar)
                 '()
                 '(target)))

; don't move graph around in telguess:
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")
(setf *do-converse* NIL)

;(setf *propn-excit-weight* .2L0) ; traditional value from sanday sims: .2L0
;(setf *propn-inhib-weight* -.01) ; default value from sanday sims: -.025L0, i.e. 1/8 of .2

(print-parameters)

;; Comment in/out persons wanted:

;(make-both-bias-crime-talker 'Vicky viral-crime-propns)
;(make-no-iff-both-bias-crime-talker 'Vicky-noiff viral-crime-propns)

(make-both-bias-crime-talker 'Becky beastly-crime-propns)
(make-no-iff-both-bias-crime-talker 'Becky-noiff beastly-crime-propns)

;(make-both-bias-crime-talker 'Bea beast-propns)
;(make-no-iff-both-bias-crime-talker 'Bea-noiff beast-propns)

;(make-both-bias-crime-talker 'Virgil virus-propns)
;(make-no-iff-both-bias-crime-talker 'Virgil-noiff virus-propns)

(print (get 'folks 'members))

(init-pop)

; crude low-level method to cause to *disbelieve* all formerly perceived propositions.
; note this has to happen after init-pop.
;(mapc #'negate-propn (get 'nay-Vicky-env 'all-propositions))
;(mapc #'negate-propn (get 'nay-Becky-env 'all-propositions))
;(mapc #'negate-propn (get 'nay-Bea-env 'all-propositions))
;(mapc #'negate-propn (get 'nay-Virgil-env 'all-propositions))

(setf *max-pop-ticks* 1)
;(popco)
