
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

(make-know-all-see-hunting 'template)
(n-persons-with-name 'template 'h 100)
(kill 'template)

(init-pop)
(print (get 'folks 'members))

(setf *do-report-analogy-nets-to-guess* t)
(write-person-graphs "graphs/casm2013/0/")
(setf *max-pop-ticks* 1)
(popco)
(write-person-graphs "graphs/casm2013/1/")
(popco-plus-t 10)
(write-person-graphs "graphs/casm2013/10/")
(popco-plus-t 10)
(write-person-graphs "graphs/casm2013/20/")
(popco-plus-t 10)
(write-person-graphs "graphs/casm2013/30/")
(popco-plus-t 10)
(write-person-graphs "graphs/casm2013/40/")
(popco-plus-t 10)
(write-person-graphs "graphs/casm2013/50/")
(popco-plus-t 50)
(write-person-graphs "graphs/casm2013/100/")
(popco-plus-t 100)
(write-person-graphs "graphs/casm2013/200/")
(popco-plus-t 200)
(write-person-graphs "graphs/casm2013/400/")
(popco-plus-t 400)
(write-person-graphs "graphs/casm2013/800/")
(popco-plus-t 400)
(write-person-graphs "graphs/casm2013/1200/")
(popco-plus-t 800)

;(quit)
