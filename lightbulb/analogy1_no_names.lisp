; analogy1.lisp
; lightbulb/tumor problem from Holyoak/Thagard 1989
; Author:   Kristen Hammack
; Vers:     1.0.0 07/04/2012 kmh - initial coding

; These are output to NetLogo:
(setf *propn-category-prefixes* '("L" "T"))
(setf *propn-category-descriptions* '("lightbulb" "tumor"))

;; first clear everything out
(mapcar #'clear-plists (get 'folks 'members))
(clear-person-nets 'folks)
(setf *the-population* 'folks)


;; CONVENTIONS:
;; Proposition names start with a prefix indicating domain, and are mixed case after that.
;; prefix key:
;;
;;      l-  : "l" propositions
;;
;;      t-  : "t" propositions
;;
;; All-uppercase can be used to flag things temporarily.


;;"L" propositions
(defvar l-simple
  '(
        (o1 (o-1) l-O-1)
        (o2 (o-2) l-O-2)
        (o3 (o-3) l-O-3)
   ))

(defvar l-2arg
    `(
        ,@l-simple
        (v1 (o-1 o-2) l-1-V1-2)
    ))

(defvar l-complex
    `(
        ,@l-2arg
        (v2 (v1 o-3) l-V1-V2-3)
    ))


;;"T" propositions
(defvar t-simple
    '(
        (r1 (r-1) t-R-1)
        (r2 (r-2) t-R-2)
        (r3 (r-3) t-R-3)
    ))

(defvar t-2arg
    `(
        ,@t-simple
        (v1 (r-1 r-2) t-1-V1-2)
    ))

(defvar t-complex
    `(
        ,@t-2arg
        (v3 (v1 r-3) t-V1-V3-3)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Put SIMILAR and SYMLINK-IF-UNITS calls here:
   
(defvar similarity '())

; Put PRESUMED and IMPORTANT calls here:
(defvar pragmatic-relations '())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODEL PERSONS

; The template:
;(make-person 'name-of-person *the-population* propns-to-be-perceived-initially
;             `((make-struc 'target 'problem '(start (,@propns-to-subst-in-here)))
;               (make-struc 'source 'problem '(start (,@propns-to-subst-in-here)))
;               ,@semantic-specs-to-subst-in-here)
;             `(@,pragmatic-relations)
;             '()) ; put 'source or 'target in list to restrict utterances to propns in that struc


;;;a1 and a2 are simple people with only 1-argument propositions
(make-person 'a1 'folks nil
             `((make-struc 'target 'problem '(start (,@t-simple)) 
                                            ;'(goals (,@tumor-goals))
                                            )
               (make-struc 'source 'problem '(start (,@l-simple))
                                            )
               ,@similarity)
             `(,@pragmatic-relations)
             '())

(make-person 'a2 'folks nil
             `((make-struc 'source 'problem '(start (,@t-simple))
                                            ;'(goals (,@tumor-goals))
                                            )
               (make-struc 'target 'problem '(start (,@l-simple))
                                            )
               ,@similarity)
             `(,@pragmatic-relations)
             '())

;;;b1 and b2 have 1 2-argument proposition in both target and source
;;;(arguments are each 1-arg propns)
(make-person 'b1 'folks nil
             `((make-struc 'source 'problem '(start (,@t-2arg))
                                            ;'(goals (,@tumor-goals))
                                            )
               (make-struc 'target 'problem '(start (,@l-2arg))
                                            )
               ,@similarity)
             `(,@pragmatic-relations)
             '())

(make-person 'b2 'folks nil
             `((make-struc 'source 'problem '(start (,@t-2arg))
                                            ;'(goals (,@tumor-goals))
                                            )
               (make-struc 'target 'problem '(start (,@l-2arg))
                                            )
               ,@similarity)
             `(,@pragmatic-relations)
             '())

;;;c1 and c2 have 1 2-arg propn where args are each 1-arg propns
;;;and 1 2-arg propn where 1 arg is 2-arg propn and the other is 1-arg propn
;;;(similar to (causes (cause effect) cause-causes-effect))
(make-person 'c1 'folks nil
             `((make-struc 'source 'problem '(start (,@t-complex))
                                            ;'(goals (,@tumor-goals))
                                            )
               (make-struc 'target 'problem '(start (,@l-complex))
                                            )
               ,@similarity)
             `(,@pragmatic-relations)
             '())

(make-person 'c2 'folks nil
             `((make-struc 'source 'problem '(start (,@t-complex))
                                            ;'(goals (,@tumor-goals))
                                            )
               (make-struc 'target 'problem '(start (,@l-complex))
                                            )
               ,@similarity)
             `(,@pragmatic-relations)
             '())


;*************************
; INITIAL SETTINGS
(setf *max-pop-ticks* 20)
(setf *do-converse* nil)             ; Whether to send utterances between persons
(setf *do-update-propn-nets* t)    ; Whether to update propn constraints from propn map units
(setf *do-report-to-netlogo* nil)  ; Whether to create file for input to NetLogo 
(setf *do-report-analogy-nets-to-guess* t)
(setf *silent-run?* t)             ; If nil, use Thagard-style verbose reporting to console
;*************************

; TEST RUN
(print (get 'folks 'members))
(format t "~%Running Model Now.~%")

; To run the model
(init-pop)
;(popco-plus-t 2)
;(write-person-graphs "graphs/")
;(print-constraints-csv 'a1)
