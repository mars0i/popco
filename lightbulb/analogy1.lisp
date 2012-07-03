; analogy1.lisp
; lightbulb

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
;;      o-  : generic origin proposition
;;	oe- : intended as earth/female origin symbol proposition
;;	os- : intended as sky/male origin symbol proposition
;;	h-  ; hunting subsistence propositions
;;      p-  ; propositions about parenting, childbirth
;;	g-  ; gathering subsistence propositions
;;
;;      l-  : 
;; All-uppercase can be used to flag things temporarily.

#|   (defvar origin-propns
   '(
     (alive (human) o-Human-Alive)
    ;; sky-origin-propns:
     (creates (s-god o-Human-Alive) os-God-Creates-Human-From-Object)
     (mysterious-process (os-God-Creates-Human) os-God-Creates-Mysteriously)
     (offends (human s-god) os-Human-Offends-God)
     (harms (s-god human) os-God-Harms-Human)
     (causes (os-Human-Offends-God os-God-Harms-Human) os-Offense-Causes-Punishment)
     (supplicates (human s-god) os-Human-Supplicates)
     (helps (s-god human) os-God-Helps-Human)
     (causes (os-Human-Supplicates os-God-Helps-Human) os-God-Rewards)
     (distant (s-god human) os-Heavenly-God)
    ;; earth-origin-propns:
     (inside (human e-god) oe-Protohuman-Inside)
     (process-from-to (oe-Protohuman-Inside o-Human-Alive) oe-Human-From-Within-God)
     (causes (e-god oe-Human-From-Within-God) oe-God-Creates-Human-From-Within)
     (natural-process (oe-God-Creates-Human-From-Within) oe-God-Creates-Naturally)
     (helps (e-god human) oe-God-Helps-Human)
     (causes (nothing oe-God-Helps-Human) oe-God-Nurtures)
     (nothing (nothing) oe-Nothing)
     (close (e-god human) oe-Earthly-God)
    ))  |#


;;Lightbulb Info and Goals, Good- and Poor-Constraint Versions
;;;TODO Figure out what to do with the goals
(defvar lightbulb-info-good
    '(
        (laser (obj-laser) fg1)
        (bulb (obj-bulb) fg2)
        (filament (obj-filament) fg3)
        (surround (obj-bulb obj-filament) fg4)
        (outside (obj-laser obj-bulb) fg5)
        (can-produce (obj-laser obj-beams-high) fg6)
        (high-intensity (obj-beams-high obj-filament) fg7)
        (can-destroy (obj-beams-high obj-bulb) fg8)
        (can-produce (obj-laser obj-beams-low) fg9)
        (low-intensity (obj-beams-low) fg10)
        (cannot-fuse (obj-beams-low obj-filament) fg11)
        (cannot-destroy (obj-beams-low obj-bulb) fg12)
    ))

(defvar lightbulb-info-poor
    '(
        (laser (obj-laser) fp1)
        (bulb (obj-bulb) fp2)
        (filament (obj-filament) fp3)
        (surround (obj-bulb obj-filament) fp4)
        (outside (obj-laser obj-bulb) fp5)
        (cannot-produce (obj-laser obj-beams-high) fp6)
        (high-intensity (obj-beams-high) fp7)
        (can-fuse (obj-beams-high obj-filament) fp8)
        (can-produce (obj-laser obj-beamsams-low) fp9)
        (low-intensity (obj-beams-low) fpl0)
        (cannot-fuse (obj-beams-low obj-filament) fp11)      
    ))

(defvar lightbulb-goals-good
    '(
        (fuse (obj-laser obj-filament) fg21)
        (not-destroyed (obj-bulb) fg22)
    ))

(defvar lightbulb-goals-poor
    '(
        (fuse (obj-laser obj-filament) fp21)
        (can-produce (obj-laser obj-beams-high) fp22)
    ))

;;Tumor Info and Goals
(defvar tumor-info
    '(
        (ray-source (obj-ray) d1)
        (tissue (obj-tissue) d2)
        (tumor (obj-tumor) d3)
        (surround (obj-tissue obj-tumor) d4)
        (outside (obj-ray obj-tissue) d5)
        (can-produce (obj-ray obj-rays-high) d6)
        (high-intensity (obj-rays-high) d7)
        (can-destroy (obj-rays-high obj-tumor) d8)
        (can-destroy (obj-rays-high obj-tissue) d9)
        (can-produce (obj-ray obj-rays-low) d10)
        (low-intensity (obj-rays-low) d11)
        (cannot-destroy (obj-rays-low obj-tumor) d12)
        (cannot-destroy (obj-rays-low obj-tissue) d13)
    ))

(defvar tumor-goals
    '(
        (destroy (obj-ray obj-tumor) d21)
        (not-destroyed (obj-tissue))
    ))


 #|  (defvar lifestyle-propns
   '(
    ;; hunting propositions:
     (feels-power (man) h-Man-Power)
     (source-of (game h-Man-Power) h-Game-Power-Source)
     (mysterious-process (h-Game-Power-Source) h-Game-Power-Mysteriously)
     (hunts-endangers (man game) h-Man-Endangers-Game)
     (harms (game man) h-Game-Harms-Man)
     (causes (h-Man-Endangers-Game h-Game-Harms-Man) h-Hunting-Is-Dangerous)
     (hunts-skillfully (man game) h-Skillful-Hunting)
     (helps (game man) h-Game-Provides)
     (causes (h-Skillful-Hunting h-Game-Provides) h-Hunting-Rewards-Skill)
     (distant-agent (game man) h-Game-Distant)
    ;; parenting-propns:
     (alive (child) p-Child-Alive)
     (intimate-agent (woman child) p-Child-Close)
     (inside (child woman) p-Protochild-Inside)
     (process-from-to (p-Protochild-Inside p-Child-Alive) p-Child-From-Within-Woman)
     (creates (woman p-Child-From-Within-Woman) p-Woman-Creates-Child-From-Within)
     (natural-process (p-Woman-Creates-Child-From-Within) p-Woman-Creates-Naturally)
     (helps (woman child) p-Woman-Helps-Child)
     (causes (nothing p-Woman-Helps-Child) p-Woman-Nurtures)
     (nothing (nothing) p-Nothing)
    ))  |#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Put SIMILAR and SYMLINK-IF-UNITS calls here:

#|  (defvar semantic-relations
  '(
    ; antonymic relationships:
    (similar 'helps 'harms (* -1 *ident-weight*))
    (similar 'feels-power 'alive (* -.75 *ident-weight*)) ; they're not antithetical, but not closely related
    ; similarity relationships, some which are kludged metaphors:
    (similar 'woman 'human (* .5 *ident-weight*))
    (similar 'man 'human (* .5 *ident-weight*))
    (similar 'distant 'distant-agent (* .5 *ident-weight*)) ; kludged metaphor [cf. Lakoff/Johnson]: physical and emotional distance are similar
    (similar 'close 'intimate-agent (* .5 *ident-weight*)) ; ditto
    (similar 'offends 'harms (* .5 *ident-weight*))
    ; within-analog antonymic relationships expressed via explicit symlinks on propns:
    (semantic-iff 'oe-Earthly-God 'os-Heavenly-God -.5) ; not contrad since e-god != s-god, but conflicting worldview
    (semantic-iff 'oe-God-Creates-Naturally 'os-God-Creates-Mysteriously -.5) ; not contrad since e-god != s-god, but conflicting worldview
    (semantic-iff 'os-Heavenly-God 'os-God-Creates-Mysteriously .1)
    ; KLUDGE: Why do I have to do the following explicitly? Shouldn't the analog stru do it?:
    (semantic-iff 'oe-God-Creates-Naturally 'oe-God-Creates-Human-From-Within .5)
    (semantic-iff 'oe-God-Nurtures 'oe-God-Creates-Human-From-Within .1)
    (semantic-iff 'oe-Human-From-Within-God 'oe-God-Creates-Human-From-Within .5)
    (semantic-iff 'oe-God-Creates-Human-From-Within 'os-God-Creates-Human-From-Object -.5)
   ))  |#
   
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
(make-person 'alex 'folks lightbulb-info-good
             `((make-struc 'target 'problem '(start (,@lightbulb-info-good)))
               (make-struc 'source 'problem '(start (,@tumor-info)))
               ,@similarity)
             `(,@pragmatic-relations)
             '()) ; empty means use all strucs' propns for conversation

(make-person 'james 'folks lightbulb-info-poor
             `((make-struc 'target 'problem '(start (,@lightbulb-info-poor)))
               (make-struc 'source 'problem '(start (,@tumor-info)))
               ,@similarity)
             `(,@pragmatic-relations)
             '())

; make persons like the first one, with names as listed:
(persons-like 'alex '(bailey chris dana))
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
(setf *do-converse* NIL)             ; Whether to send utterances between persons
(setf *do-update-propn-nets* t)    ; Whether to update propn constraints from propn map units
(setf *do-report-to-netlogo* t)  ; Whether to create file for input to NetLogo 
(setf *do-report-analogy-nets-to-guess* t)
(setf *silent-run?* t)             ; If nil, use Thagard-style verbose reporting to console
;*************************

; TEST RUN
(print (get 'folks 'members))
(format t "~%See end of input file for commands to run the model.~%")

; To run the model, you can use these commands, for example:
;(init-pop)
;(popco)
