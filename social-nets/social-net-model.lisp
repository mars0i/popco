;social-net-model.lisp
;model with persons and groups using Holyoak/Thagard's lightbulb/tumor problem (1989)
;Author:    Kristen Hammack
;Vers:      1.0.0 12/2012 kmh - initial coding

(myload "social-nets/social-net-functions.lisp")
(myload "lightbulb/lightbulb-vars.lisp")



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
(make-person 'alex 'alpha '()
             `((make-struc 'target 'problem '(start (,@lightbulb-info-poor)) 
                                            ;'(goals (,@lightbulb-goals-poor))
                                            )
               (make-struc 'source 'problem '(start (,@tumor-info)) 
                                            ;'(goals (,@tumor-goals))
                                            )
               ,@similarity)
             `(,@pragmatic-relations)
             '()
             '(alpha bravo)
             *num-ppl-talking*)
(persons-like 'alex '(bailey chris dana))


(make-person 'james 'bravo '()
             `((make-struc 'target 'problem '(start (,@lightbulb-common)) 
                                            ;'(goals (,@lightbulb-goals-common))
                                            )
               (make-struc 'source 'problem '(start (,@tumor-info)) 
                                            ;'(goals (,@tumor-goals))
                                            )
               ,@similarity)
             `(,@pragmatic-relations)
             '()
             '(alpha bravo charlie)
             *num-ppl-talking*)
(persons-like 'james '(kristen mary logan))


(make-person 'zander 'charlie lightbulb-common
             `((make-struc 'target 'problem '(start (,@lightbulb-info-good)) 
                                            ;'(goals (,@lightbulb-goals-good))
                                            )
               (make-struc 'source 'problem '(start (,@tumor-info)) 
                                            ;'(goals (,@tumor-goals))
                                            )
               ,@similarity)
             `(,@pragmatic-relations)
             '()
             '(bravo charlie)
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
;(merge-pops '(alpha bravo charlie))
(merge-groups '(alpha bravo charlie))
(init-pop)
(print (get 'folks 'members))

(popco)
