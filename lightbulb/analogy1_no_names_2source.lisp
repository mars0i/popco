; analogy1.lisp
; lightbulb/tumor problem from Holyoak/Thagard 1989
; Author:   Kristen Hammack
; Vers:     1.0.0 07/04/2012 kmh - initial coding

; These are output to NetLogo:
;(setf *propn-category-prefixes* '("L" "T"))
;(setf *propn-category-descriptions* '("lightbulb" "tumor"))

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
(defvar l-1
   '(
        (o1 (o-1) l-O-1)
        (o2 (o-2) l-O-2)
        (o3 (o-3) l-O-3)
   ))
(defvar l-2
   '(
        (o4 (o-4) l-O-4)
        (o5 (o-5) l-O-5)
        (o6 (o-6) l-O-6)
   ))

(defvar l-both
   `(
        ,@l-1
        ,@l-2
   ))


;;"T" propositions
(defvar t-1
    '(
        (r1 (r-1) t-R-1)
        (r2 (r-2) t-R-2)
        (r3 (r-3) t-R-3)
    ))

#|  (defvar t-2
   '(
        (r4 (r-4) t-O-4)
        (r5 (r-5) t-O-5)
        (r6 (r-6) t-O-6)
   ))

(defvar t-both
    `(
        ,@t-1
        ,@t-2
    ))  |#


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


;;;a1 has 2 separate sources; a2 has 1 source with all of the propns from a1's 2 sources
(make-person 'a1 'folks nil
             `((make-struc 'target 'problem '(start (,@t-1)) 
                                            )
               (make-struc 'source1 'problem '(start (,@l-1))
                                            )
               (make-struc 'source2 'problem '(start (,@l-2))
                                            )
               ,@similarity)
             `(,@pragmatic-relations)
             '())

(make-person 'a2 'folks nil
             `((make-struc 'target 'problem '(start (,@t-1))
                                            )
               (make-struc 'source 'problem '(start (,@l-both))
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
(popco-plus-t 2)
;(write-person-graphs "graphs/")
;(print-constraints-csv 'a1)
