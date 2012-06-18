; HTsOriginalNicaraguaForPOPCO.lisp

; Attempt to recreate original Nicaragua/Contras example from Holyoak
; and Thagard 1989 "Analog Retrieval by Constraint Satisfaction",
; embedding it in a single person (which does not engage in communication).

;; first clear everything out
(mapcar #'clear-plists (get 'politicians 'members))
(clear-person-nets 'politicians)

(defvar pols 'politicians)

;*************************
(SETF *DONT-CONVERSE* T)            ; don't allow people to converse
(SETF *DONT-UPDATE-PROPN-NETS* T)   ; don't create proposition nets--only ACME nets
(SETF *MAX-GENERATION* 1)           ; run only one generation, with
(SETF *MAX-TIMES* 150)              ; sufficient cycles to allow setlting
;; or:
;(SETF *MAX-TIMES* 1)                ; run one net cycle per generation, with
;(SETF *MAX-GENERATION* 150)         ; sufficient generations to allow settling
(SETF *SILENT-RUN?* NIL)            ; let ACME produce its noisy output
;*************************

;; Description of structure of relations between propositions below:
;; - There are no higher-order propositions.
;; - Source contains two patterns of propositions which are exactly parallel
;;   to the pattern in target.
;; - Each pattern contains exactly the same predicates, but different objects.
;;   Thus there will connections from the semantic unit to the predicate map nodes.
;;   More specifically, polly_x=x has a link to SPECIAL with weight .1 .
;;   (Note how smal that weight is, even though the predicates are assumed to mean
;;   exactly the same thing.)
;; - There is exactly one object which is shared across patterns.  Note that objects
;;   don't receive emphasis from the semantic node.
;; - Reading down through the propositions within a pattern, each proposition has
;;   exactly one object which appears in a previous proposition.
;; - There are three non-query propositions and three query propositions.  The one
;;   unary predicate appears in one of the non-query propositions.  The rest are
;;   binary, except for the last, which might as well be binary, since the extra
;;   argument is US, which appears nowhere else within each pattern.  As noted above,
;;   object identity across analog structures doesn't produce any kind of extra
;;   semantic emphasis to the map, since object names are just that--names.
;;   Indiscernability of identicals doesn't play a role--at least in this model.
;; - There is exactly one PRESUMED statement, concerning one of the query maps,
;;   which appears in exactly one proposition.

(make-person 
  'polly 'politicians '()
  '((make-struc 'target 'problem
                '(start 
                   ((country (Nicaragua) c1) 
                    (govern (Sandinistas Nicaragua) c2) 
                    (aim-to-overthrow (Contras Sandinistas) c3) 
                    (terrorists? (Contras Contra-terror?) c4) 
                    (freedom-fighters? (Contras Contra-freedom?) c5) 
                    (should-support? (US Contras support?) c6))))
    (make-struc 'source 'problem
                '(start 
                   ((country (Hungary) h1) 
                    (govern (communists Hungary) h2) 
                    (aim-to-overthrow (Hungarians communists) h3) 
                    (terrorists? (Hungarians terrorists-no) h4) 
                    (freedom-fighters? (Hungarians freedom-fighters-yes) h5) 
                    (should-support? (US Hungarians support-yes) h6)

                    (country (Israel) i1) 
                    (govern (Israelis Israel) i2) 
                    (aim-to-overthrow (PLO Israelis) i3) 
                    (terrorists? (PLO terrorists-yes) i4) 
                    (freedom-fighters? (PLO freedom-fighters-no) i5) 
                    (should-support? (US PLO support-no) i6))))
    (presumed '(support?=support-yes))))

(create-nets 'politicians)

;(setf *silent-run?* t)
; run as many generations as specified above:
(popco pols)

; now do one more generation for the sake of output:
;(setf *max-generation* (1+ *max-generation*))
;(setf *silent-run?* nil)
;(popco1 pols)

;(print-values) ; check parameter settings
