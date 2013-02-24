;social-net-one-pop.lisp
;model for use with networking functions with only one group
;using Holyoak/Thagard's lightbulb/tumor problem (1989)
;Author:    Kristen Hammack
;Vers:      1.0.0 12/2012 kmh - initial coding

;(myload "networking/networking2-functions.lisp")

; These are output to NetLogo:
(setf *propn-category-prefixes* '("L" "LG" "LP" "T"))
(setf *propn-category-descriptions* '("lightbulb-common" "lightbulb-good" "lightbulb-poor" "tumor"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;   THE MODEL   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
(defvar lightbulb-common
  '(
        (laser (obj-laser) l-Obj-Laser)
        (bulb (obj-bulb) l-Obj-Bulb)
        (filament (obj-filament) l-Obj-Filament)
        (surround (obj-bulb obj-filament) l-Bulb-Surrounds-Filament)
        (outside (obj-laser obj-bulb) l-Laser-Outside-Bulb)
   ))

(defvar lightbulb-info-good
    `(
        ,@lightbulb-common
        (can-produce (obj-laser obj-beams-high) lg-Laser-Can-Produce-High-Beams)
        (can-fuse (obj-beams-high obj-filament) lg-High-Beams-Can-Fuse-Filament)
        (high-intensity (obj-beams-high) lg-High-Beams-Are-High-Intensity)
        (can-destroy (obj-beams-high obj-bulb) lg-High-Beams-Can-Destroy-Bulbs)
        (can-produce (obj-laser obj-beams-low) lg-Laser-Can-Produce-Low-Beams)
        (low-intensity (obj-beams-low) lg-Low-Beams-Are-Low-Intensity)
        (cannot-fuse (obj-beams-low obj-filament) lg-Low-Beams-Cannot-Fuse-Filament)
        (cannot-destroy (obj-beams-low obj-bulb) lg-Low-Beams-Cannot-Destroy-Bulb)
    ))

;(defvar all-lightbulb-good (append lightbulb-common lightbulb-info-good))
;(defvar all-lightbulb-good `(,@lightbulb-common ,@lightbulb-info-good))

(defvar lightbulb-info-poor
    `(
        ,@lightbulb-common
        (cannot-produce (obj-laser obj-beams-high) lp-Laser-Cannot-Produce-High-Beams)
        (high-intensity (obj-beams-high) lp-High-Beams-Are-High-Intensity)
        (can-fuse (obj-beams-high obj-filament) lp-High-Beams-Can-Fuse-Filament)
        (can-produce (obj-laser obj-beams-low) lp-Laser-Can-Produce-Low-Beams)
        (low-intensity (obj-beams-low) lp-Low-Beams-Are-Low-Intensity)
        (cannot-fuse (obj-beams-low obj-filament) lp-Low-Beams-Cannot-Fuse-Filament)      
    ))

;;;This doesn't seem worth it with only 2 goals in each, but it's probably good practice...
(defvar lightbulb-goals-common
    '(
        (fuse (obj-laser obj-filament) l-Laser-Fuse-Filament)))

(defvar lightbulb-goals-good
    `(
        ,@lightbulb-goals-common
        (not-destroyed (obj-bulb) lg-Do-Not-Destroy-Bulb)
    ))

(defvar lightbulb-goals-poor
    `(
        ,@lightbulb-goals-common
        (can-produce (obj-laser obj-beams-high) lp-Laser-Produce-High-Beams)
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
        (high-intensity (obj-rays-high) t-High-Rays-Are-High-Intensity)
        (can-destroy (obj-rays-high obj-tumor) t-High-Rays-Can-Destroy-Tumor)
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
;             '() ; put 'source or 'target in list to restrict utterances to propns in that struc
;	      '(groups-to talk-to)) ;a list of the groups this person talks to (defaults to only own group)
;	      num-listeners ;integer variable


;; make the people
;for right now, just using a global for the number of listeners each person has
(defvar *num-ppl-talking* 1)
(make-person 'alex 'folks '()
             `((make-struc 'target 'problem '(start (,@lightbulb-info-poor)) 
                                            ;'(goals (,@lightbulb-goals-poor))
                                            )
               (make-struc 'source 'problem '(start (,@tumor-info)) 
                                            ;'(goals (,@tumor-goals))
                                            )
               ,@similarity)
             `(,@pragmatic-relations)
             '()
             '(folks)
             *num-ppl-talking*)
(persons-like 'alex '(bailey chris dana))


(make-person 'james 'folks '()
             `((make-struc 'target 'problem '(start (,@lightbulb-common)) 
                                            ;'(goals (,@lightbulb-goals-common))
                                            )
               (make-struc 'source 'problem '(start (,@tumor-info)) 
                                            ;'(goals (,@tumor-goals))
                                            )
               ,@similarity)
             `(,@pragmatic-relations)
             '()
             '(folks)
             *num-ppl-talking*)
(persons-like 'james '(kristen mary logan))


(make-person 'zander 'folks lightbulb-common
             `((make-struc 'target 'problem '(start (,@lightbulb-info-good)) 
                                            ;'(goals (,@lightbulb-goals-good))
                                            )
               (make-struc 'source 'problem '(start (,@tumor-info)) 
                                            ;'(goals (,@tumor-goals))
                                            )
               ,@similarity)
             `(,@pragmatic-relations)
             '()
             '(folks)
             *num-ppl-talking*)
(persons-like 'zander '(yasamin xavier wilson))




;*************************
; INITIAL SETTINGS
(setf *max-pop-ticks* 20)
(setf *the-population* 'folks)
(setf *do-converse* t)             ; Whether to send utterances between persons
(setf *do-report-to-netlogo* t)  ; Whether to create file for input to NetLogo
(setf *silent-run?* t)             ; If nil, use Thagard-style verbose reporting to console
;*************************


;;;To run the model
(init-pop)
(print (get 'folks 'members))

(popco)
