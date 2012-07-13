; analogy1simple.lisp
; SUPER SIMPLIFIED lightbulb/tumor problem from Holyoak/Thagard 1989
; Author:   Kristen Hammack
; Vers:     1.0.0 07/10/2012 kmh - initial coding

; These are output to NetLogo:
(setf *propn-category-prefixes* '("L" "T"))
(setf *propn-category-descriptions* '("lightbulb" "tumor"))
(setf *netlogo-output-name* "../data/analogy1SimpleSwitchedNetLogoData.txt")

;; first clear everything out
(mapcar #'clear-plists (get 'folks 'members))
(clear-person-nets 'folks)
(setf *the-population* 'folks)


;; CONVENTIONS:
;; Proposition names start with a prefix indicating domain, and are mixed case after that.
;; prefix key:
;;
;;      l-  : lightbulb propositions
;;
;;      t-  : tumor propositions
;;
;; All-uppercase can be used to flag things temporarily.


;;Lightbulb Info
(defvar laser-info
  '(
        (filament (obj-filament) l-Obj-Filament)
   ))

(defvar bulb-info
   '(
        (bulb (obj-bulb) l-Obj-Bulb)
        (laser (obj-laser) l-Obj-Laser)
   ))

(defvar lightbulb-info
   `(
        ,@laser-info
        ,@bulb-info
   ))



;;Tumor Info
(defvar tumor-info
    '(
        (ray-source (obj-ray) t-Obj-Ray)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Put SIMILAR and SYMLINK-IF-UNITS calls here:
   
(defvar similarity 
    '(
        (similar 'ray-source 'laser (* .08 *ident-weight*))
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
(make-person 'alex 'folks lightbulb-info
             `((make-struc 'target 'problem '(start (,@tumor-info)))
               (make-struc 'source 'problem '(start (,@laser-info)))
               (make-struc 'source 'problem '(start (,@bulb-info)))
               ,@similarity)
             `(,@pragmatic-relations)
             '())

(make-person 'bailey 'folks lightbulb-info
            `((make-struc 'target 'problem '(start (,@bulb-info)))
              (make-struc 'source 'problem '(start (,@tumor-info)))
              ,@similarity)
            `(,@pragmatic-relations)
            '()) 


;*************************
; INITIAL SETTINGS
(setf *max-pop-ticks* 20)
(setf *do-converse* nil)             ; Whether to send utterances between persons
(setf *do-update-propn-nets* t)    ; Whether to update propn constraints from propn map units
(setf *do-report-to-netlogo* t)  ; Whether to create file for input to NetLogo 
(setf *do-report-analogy-nets-to-guess* t)
(setf *silent-run?* t)             ; If nil, use Thagard-style verbose reporting to console
;*************************

; TEST RUN
(print (get 'folks 'members))
(format t "~%Running Model Now.~%")

; To run the model
(init-pop)
(popco1)
(popco1)
(write-person-graphs "../data/guess/")
