;make-dirpairs-model.lisp
;model for testing directed pairs functions using Holyoak/Thagard's lightbulb/tumor problem (1989)
;Author:    Kristen Hammack
;Vers:      1.0.0 01/2013 kmh - initial coding

(myload "social-nets/social-net-functions.lisp")
(myload "lightbulb/lightbulb-vars.lisp")

;*************************
; INITIAL SETTINGS
(setf *max-pop-ticks* 5)
(setf *the-population* 'folks)   ; NOW REQUIRED TO BE SET BEFORE ANY CALL TO MAKE-PERSON
(setf *do-converse* t)           ; Whether to send utterances between persons
(setf *do-report-to-netlogo* t)  ; Whether to create file for input to NetLogo
(setf *silent-run?* t)           ; If nil, use Thagard-style verbose reporting to console
(setf *do-report-propns-to-csv* nil)
(setf *do-update-propn-nets* nil)
(setf *time-runs* nil)
;*************************


(make-person 'p01 nil nil
             `((make-struc 'target 'problem '(start (,@lightbulb-common)))
               (make-struc 'source 'problem '(start (,@tumor-info)))
               ,@similarity)
             `(,@pragmatic-relations)
             '())

(persons-like 'p01 (make-names2 'p 2 5))

(persons-like 'p01 (make-names2 'o 1 10))


(make-ungrouped-directed-pairs '((p01 p02) (p03 p04) (p05 p06)
                                 (p01 p04) (p02 p05) (p03 p06)
                                 (p04 p01) (p06 p03) (p06 p01)))



(make-person 'pundit nil lightbulb-info-good
             `((make-struc 'target 'problem '(start (,@lightbulb-info-good)))
               (make-struc 'source 'problem '(start (,@tumor-info)))
               ,@similarity)
             `(,@pragmatic-relations)
             '()
             '(folks)
             5)


(init-pop)
(print (get-members 'folks))
(popco)

(put 'pundit 'num-listeners 0)
(popco-plus-t 45)