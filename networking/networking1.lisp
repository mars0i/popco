;networking1.lisp
;sandbox for networks in POPCO
;Author:    Kristen Hammack
;Vers:      1.0.0 12/2012 kmh - initial coding


;; These are output to NetLogo:
(setf *propn-category-prefixes* '("S"))
(setf *propn-category-descriptions* '("Stuff"))

;; first clear everything out
(mapcar #'clear-plists (get 'folks 'members))
(clear-person-nets 'folks)

;;TODO: this is going to have to get fixed for the groups to work...
(setf *the-population* 'folks)


;; CONVENTIONS:
;; Proposition names start with a prefix indicating domain, and are mixed case after that.
;; prefix key:
;;
;;      s-  : stuff
;;
;; All-uppercase can be used to flag things temporarily.

;;Stuff info
(defvar stuff
  '(
        (something (obj-something) s-Obj-Something)
   ))

(defvar other
   `(
        (else (obj-else) s-Obj-Else)
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Put SIMILAR and SYMLINK-IF-UNITS calls here:
   
(defvar similarity 
    '(
        (similar 'something 'else (* -0.8 *ident-weight*))
    ))

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

; make one model person
(make-person 'alex 'alpha nil
             `((make-struc 'target 'problem '(start (,@other)) 
                                            ;'(goals (,@other-goals))
                                            )
               (make-struc 'source 'problem '(start (,@stuff)) 
                                            ;'(goals (,@stuff-goals))
                                            )
               ,@similarity)
             `(,@pragmatic-relations)
             '())
(persons-like 'alex '(bailey chris dana))


(make-person 'james 'bravo nil
             `((make-struc 'target 'problem '(start (,@other)) 
                                            ;'(goals (,@other-goals))
                                            )
               (make-struc 'source 'problem '(start (,@stuff)) 
                                            ;'(goals (,@stuff-goals))
                                            )
               ,@similarity)
             `(,@pragmatic-relations)
             '())

(persons-like 'james '(kristen mary logan))

(make-person 'zander 'charlie nil
             `((make-struc 'target 'problem '(start (,@other)) 
                                            ;'(goals (,@other-goals))
                                            )
               (make-struc 'source 'problem '(start (,@stuff)) 
                                            ;'(goals (,@stuff-goals))
                                            )
               ,@similarity)
             `(,@pragmatic-relations)
             '())

(persons-like 'zander '(yasamin xavier wilson))



(defun merge-pops (pop1 pop2)
  (concatenate (get pop1 'members) (get pop2 'members))
  )



;*************************
; INITIAL SETTINGS
(setf *max-pop-ticks* 20)
(setf *do-converse* nil)             ; Whether to send utterances between persons
(setf *do-update-propn-nets* t)    ; Whether to update propn constraints from propn map units
(setf *do-report-to-netlogo* t)  ; Whether to create file for input to NetLogo 
(setf *do-report-analogy-nets-to-guess* t)
(setf *silent-run?* t)             ; If nil, use Thagard-style verbose reporting to console
;*************************


(print (get (get 'alex 'group) 'members))


(setf 'ab (merge-pops 'alpha 'bravo))
(setf 'bc (merge-pops 'bravo 'charlie))

(run-population-once 'ab)
(run-population-once 'bc)




