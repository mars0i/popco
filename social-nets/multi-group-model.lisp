;multi-group-model.lisp
;model with persons being in multiple groups using Holyoak/Thagard's lightbulb/tumor problem (1989)
;Author:    Kristen Hammack
;Vers:      1.0.0 12/2012 kmh - initial coding

(myload "social-nets/social-net-functions.lisp")
(myload "lightbulb/lightbulb-vars.lisp")

;*************************
; INITIAL SETTINGS
(setf *max-pop-ticks* 20)
(setf *the-population* 'folks)   ; NOW REQUIRED TO BE SET BEFORE ANY CALL TO MAKE-PERSON
(setf *do-converse* t)           ; Whether to send utterances between persons
(setf *do-report-to-netlogo* t)  ; Whether to create file for input to NetLogo
(setf *silent-run?* t)           ; If nil, use Thagard-style verbose reporting to console
;*************************

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODEL PERSONS

; The template:
;(make-person 'name-of-person group propns-to-be-perceived-initially
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


;;;THE GROUPS:
;;;A:   talks-to A	target poor
;;;B:   talks-to A,B,C	target common
;;;C:   talks-to C	target good
;;;
;;;PEOPLE IN MULTIPLE GROUPS:
;;; 3rd person of each group is also in D
;;; and has these characteristics:
;;;A,D: talks-to D	target poor
;;;B,D: talks-to D	target common
;;;C,D: talks-to D	target good
;;;
;;;introvert   in groups A,B,C,D   talks-to nil  target nil
;;;
;;;PEOPLE IN NO GROUPS:
;;;hermit talks-to nil  target nil
;;;pundit talks-to *the-population*   target good   percieved good   num-listening 3


(make-person 'hermit nil nil
             `((make-struc 'target 'problem '(start nil))
               (make-struc 'source 'problem '(start nil))
               nil)
             nil
             '())

(make-person 'pundit nil lightbulb-info-good
             `((make-struc 'target 'problem '(start (,@lightbulb-info-good)))
               (make-struc 'source 'problem '(start (,@tumor-info)))
               ,@similarity)
             `(,@pragmatic-relations)
             '()
             '(folks)
             3)



(make-person 'alpha01 'alpha '()
             `((make-struc 'target 'problem '(start (,@lightbulb-info-poor)))
               (make-struc 'source 'problem '(start (,@tumor-info)))
               ,@similarity)
             `(,@pragmatic-relations)
             '()
             '(alpha)
             *num-ppl-talking*)
(persons-like 'alpha01 (make-names2 'alpha 2 2))


(make-person 'bravo01 'bravo '()
             `((make-struc 'target 'problem '(start (,@lightbulb-common)))
               (make-struc 'source 'problem '(start (,@tumor-info)))
               ,@similarity)
             `(,@pragmatic-relations)
             '()
             '(alpha bravo charlie)
             *num-ppl-talking*)
(persons-like 'bravo01 (make-names2 'bravo 2 2))


(make-person 'charlie01 'charlie '()
             `((make-struc 'target 'problem '(start (,@lightbulb-info-good)))
               (make-struc 'source 'problem '(start (,@tumor-info)))
               ,@similarity)
             `(,@pragmatic-relations)
             '()
             '(charlie)
             *num-ppl-talking*)
(persons-like 'charlie01 (make-names2 'charlie 2 2))


(make-person 'introvert '(alpha bravo charlie delta) '() 
             `((make-struc 'target 'problem '(start nil))
               (make-struc 'source 'problem '(start nil))
               ,@similarity)
             `(,@pragmatic-relations)
             '()
             '()
             0)



(put-in-group 'alpha03 'delta)
(put 'alpha03 'groups '(alpha delta))
(put 'alpha03 'talks-to '(delta))

(put-in-group 'bravo03 'delta)
(put 'bravo03 'groups '(alpha delta))
(put 'bravo03 'talks-to '(delta))

(put-in-group 'charlie03 'delta)
(put 'charlie03 'groups '(alpha delta))
(put 'charlie03 'talks-to '(delta))




;;;To run the model
(merge-groups '(alpha bravo charlie delta))
(init-pop)
(print (get 'folks 'members))

;(popco)
