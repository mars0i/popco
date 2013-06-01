
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
(setf *do-converse* nil)

(make-know-all-see-hunting 'template)
(n-persons-with-name 'template 'h 1)
(kill 'template)

(init-pop)
(print (get 'folks 'members))

(setf *max-pop-ticks* 2000)
(popco)
(quit)
