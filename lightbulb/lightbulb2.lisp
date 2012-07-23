; analogy2.lisp
; lightbulb/tumor problem from Holyoak/Thagard 1989
; simplified by removing the bad-constraint version of lightbulb,
;  and rearranged lines so that corresponding propositions in both
;  analogs are in the same order.
; Author:   Kristen Hammack, with mods by Marshall Abrams
; Vers:     1.0.0 07/23/2012 ma

; These are output to NetLogo:
(setf *propn-category-prefixes* '("L" "LG" "LP" "T"))
(setf *propn-category-descriptions* '("lightbulb-common" "lightbulb-good" "lightbulb-poor" "tumor"))

;; first clear everything out
(mapcar #'clear-plists (get 'folks 'members))
(clear-person-nets 'folks)
(setf *the-population* 'folks)


;; CONVENTIONS:
;; Proposition names start with a prefix indicating domain, and are mixed case after that.
;; prefix key:
;;
;;      l-  : generic lightbulb propositions
;;      lg- : propositions from the good-constraint version of the lightbulb problem
;;      lp- : propositions from the poor-constraint version of the lightbulb problem
;;
;;      t-  : tumor propositions
;;
;; All-uppercase can be used to flag things temporarily.


;;Lightbulb Info and Goals, Good- and Poor-Constraint Versions
(defvar lightbulb-info
  '(
        (laser (obj-laser) l-Obj-Laser)
        (bulb (obj-bulb) l-Obj-Bulb)
        (filament (obj-filament) l-Obj-Filament)
        (surround (obj-bulb obj-filament) l-Bulb-Surrounds-Filament)
        (outside (obj-laser obj-bulb) l-Laser-Outside-Bulb)
        (can-produce (obj-laser obj-beams-high) lg-Laser-Can-Produce-High-Beams)
        (can-fuse (obj-beams-high obj-filament) lg-High-Beams-Can-Fuse-Filament)
        (high-intensity (obj-beams-high) lg-High-Beams-Are-High-Intensity)
        (can-destroy (obj-beams-high obj-bulb) lg-High-Beams-Can-Destroy-Bulbs)
        (can-produce (obj-laser obj-beams-low) lg-Laser-Can-Produce-Low-Beams)
        (low-intensity (obj-beams-low) lg-Low-Beams-Are-Low-Intensity)
        (cannot-fuse (obj-beams-low obj-filament) lg-Low-Beams-Cannot-Fuse-Filament)
        (cannot-destroy (obj-beams-low obj-bulb) lg-Low-Beams-Cannot-Destroy-Bulb)
    ))

;;;This doesn't seem worth it with only 2 goals in each, but it's probably good practice...
(defvar lightbulb-goals
    '(
        (fuse (obj-laser obj-filament) l-Laser-Fuse-Filament)
        (not-destroyed (obj-bulb) lg-Do-Not-Destroy-Bulb)
    ))


;;Tumor Info and Goals
(defvar tumor-info
    '(
        (ray-source (obj-ray) t-Obj-Ray)
        (tissue (obj-tissue) t-Obj-Tissue)
        (tumor (obj-tumor) t-Obj-Tumor)
        (surround (obj-tissue obj-tumor) t-Tissue-Surrounds-Tumor)
        (outside (obj-ray obj-tissue) t-Ray-Outside-Tissue)
        (can-produce (obj-ray obj-rays-high) t-Ray-Can-Produce-High-Rays)
        (can-destroy (obj-rays-high obj-tumor) t-High-Rays-Can-Destroy-Tumor)
        (high-intensity (obj-rays-high) t-High-Rays-Are-High-Intensity)
        (can-destroy (obj-rays-high obj-tissue) t-High-Rays-Can-Destroy-Tissue)
        (can-produce (obj-ray obj-rays-low) t-Ray-Can-Produce-Low-Rays)
        (low-intensity (obj-rays-low) t-Low-Rays-Are-Low-Intensity)
        (cannot-destroy (obj-rays-low obj-tumor) t-Low-Rays-Cannot-Destroy-Tumor)
        (cannot-destroy (obj-rays-low obj-tissue) t-Low-Rays-Cannot-Destroy-Tissue)
    ))


(defvar tumor-goals
    '(
        (destroy (obj-ray obj-tumor) t-Ray-Destroy-Tumor)
        (not-destroyed (obj-tissue) t-Do-Not-Destroy-Tissue)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Put SIMILAR and SYMLINK-IF-UNITS calls here:
   
(defvar similarity 
    '(
        (similar 'ray-source 'laser (* .8 *ident-weight*))
        (similar 'filament 'tumor (* .1 *ident-weight*))
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
             `((make-struc 'target 'problem '(start (,@tumor-info)) 
                                            ;'(goals (,@tumor-goals))
                                            )
               (make-struc 'source 'problem '(start (,@lightbulb-info)) 
                                            ;'(goals (,@lightbulb-goals))
                                            )
               ,@similarity)
             `(,@pragmatic-relations)
             '())
(persons-like 'alex '(bailey chris dana))

; make persons like the first one, with names as listed:
;(persons-like 'alex '(bailey chris dana))
(persons-like 'james '(kristen logan mary))

; Make five persons like the first one, using first one's name 
; as starting point, and numbering them starting from 1:
;(n-persons 'alex 5 1)

; Make five persons like the first one, using a new name quinn
; as a starting point, and numbering them starting from 1:
;(n-persons-with-name 'alex 'quinn 5)


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
;(popco)
