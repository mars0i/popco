; like earthorigindifuses.lisp, but without the persons who have the
; earth origin propns--that idea never arises.  

(load "nosettle")      ; don't allow networks to settle, ever, since subtle differences are amplified by communication
(load "start") ; popco
(defun report-progress-to-console ()) ; make this into a no-op - for batch jobs

(load "sanday/casm052013/parenting18analogsAsBiases2")     ; library
(load "sanday/casm052013/parenting18JuneJuly2012persons")  ; library

;(setf *data-dir* "data/huntingsoc")

(setf *do-report-to-netlogo* nil)
; don't move graph around in telguess:
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")
(setf *do-converse* t)

; make population of 100 persons ignorant of earth origin
(make-earthless-person 'template)
(n-persons-with-name 'template 'h 100)
(kill 'template)

(init-pop)
(print (get 'folks 'members))

; run for 1000 ticks with hunting salience, while earth-origin-propns DON'T spread sotto voce s.t.s.
(setf *max-pop-ticks* 1000)
(popco)
(drop-salience) ; removes all salience links from all persons

(mapc #'parentize-person (get *the-population* 'members))
(mapc #'dehunterize-person (get *the-population* 'members))

;(let ((to-flip (random-subset 75 (get *the-population* 'members))))
;  (mapc #'parentize-person to-flip)
;  (mapc #'dehunterize-person to-flip))

(popco-plus-t 2000)

(quit)
