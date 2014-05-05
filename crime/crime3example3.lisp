;;;; EXAMPLE FOR COMPARISON WITH POPCO2

(load "nosettle")      ; don't allow networks to settle, ever, since subtle differences are amplified by communication
(load "crime/crime3")

(setf *do-converse* NIL)
(setf *do-report-propns-to-csv* T)
(setf *do-report-to-netlogo* nil)
(setf *do-update-propn-nets-from-propn-nets* nil)
; don't move graph around in telguess:
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")

(make-both-bias-crime-talker  'bo crime-propns 'folks '(folks))
(make-beast-bias-crime-talker 'be crime-propns 'folks '(folks))
(make-virus-bias-crime-talker 'bv crime-propns 'folks '(folks))

(setf *max-pop-ticks* 100)
(init-pop)
(print (get 'folks 'members))

(popco)
