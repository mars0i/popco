
(load "nosettle")      ; don't allow networks to settle, ever, since subtle differences are amplified by communication
(load "crime/crime3")

(setf *do-report-to-netlogo* nil)
(setf *do-report-propns-to-csv* nil)

(setf *DO-UPDATE-PROPN-NETS-FROM-PROPN-NETS* NIL)

(defvar *group-size* 32)

; don't move graph around in telguess:
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")
(setf *do-converse* NIL)

;(make-no-bias-crime-talker 'aa crime-propns 'pundits '(VIBIs BEBIs BOBIs NOBIs))  ; pundit

(make-virus-bias-crime-talker 'jov '() 'folks '(folks))
(n-persons-with-name 'jov 'vi *group-size*)
;(kill 'temp-vi)

(make-beast-bias-crime-talker 'job '() 'folks '(folks))
(n-persons-with-name 'job 'be *group-size*)
;(kill 'temp-be)

(make-both-bias-crime-talker 'jo '() 'folks '(folks))
(n-persons-with-name 'jo 'bo *group-size*)
;(kill 'temp-bo)

(setf *max-pop-ticks* 1000)
(init-pop)
(print (get 'folks 'members))

(gc :full t) ; garbage collect first for greater uniformity in time trials
(popco)
(quit)
