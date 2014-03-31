;;;; Generate several analogy net variants for POPCO 2's comparison tests

(load "nosettle")      ; don't allow networks to settle, ever, since subtle differences are amplified by communication
(load "crime/crime3")

(setf *do-report-to-netlogo* nil)
(setf *do-report-propns-to-csv* nil)

(setf *DO-CONVERSE* NIL) ;; no conversation--everyone is isolated

; don't move graph around in telguess:
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")


;; There are 2^4 = 16 combinations of the four sets of propns.  We generate only some of those combinations below.

;; all propns
(make-person 'crime-living 'folks '() `((make-struc 'target 'problem '(start (,@viral-crime-propns ,@beastly-crime-propns)))
                                        (make-struc 'source 'problem '(start (,@virus-propns ,@beast-propns)))
                                        ,@semantic-relations) '() '())
;; all crime, beast only
(make-person 'crime-beast 'folks '() `((make-struc 'target 'problem '(start (,@viral-crime-propns ,@beastly-crime-propns)))
                                       (make-struc 'source 'problem '(start (,@beast-propns)))
                                       ,@semantic-relations) '() '())
;; all crime, virus only
(make-person 'crime-virus 'folks '() `((make-struc 'target 'problem '(start (,@viral-crime-propns ,@beastly-crime-propns)))
                                       (make-struc 'source 'problem '(start (,@virus-propns)))
                                       ,@semantic-relations) '() '())
;; all crime, no living
(make-person 'crime-noliving 'folks '() `((make-struc 'target 'problem '(start (,@viral-crime-propns ,@beastly-crime-propns)))
                                          (make-struc 'source 'problem '(start ()))
                                          ,@semantic-relations) '() '())
;; viral crime, all living
(make-person 'viralcrime-living 'folks '() `((make-struc 'target 'problem '(start (,@viral-crime-propns)))
                                             (make-struc 'source 'problem '(start (,@virus-propns ,@beast-propns)))
                                             ,@semantic-relations) '() '())
;; beastly crime, all living
(make-person 'beastlycrime-living 'folks '() `((make-struc 'target 'problem '(start (,@beastly-crime-propns)))
                                               (make-struc 'source 'problem '(start (,@virus-propns ,@beast-propns)))
                                               ,@semantic-relations) '() '())
;; no crime, all living
(make-person 'nocrime-living 'folks '() `((make-struc 'target 'problem '(start ()))
                                          (make-struc 'source 'problem '(start (,@virus-propns ,@beast-propns)))
                                          ,@semantic-relations) '() '())

;; allow a little bit of running just to get past the first tick in which things aren't fully set up
;; no weights will change, though
(setf *max-pop-ticks* 5)
(init-pop)
(print (get 'folks 'members))

(popco*)

(mapc #'list-analogy-constraints-for-popco2-comparison-to-person-file (get 'folks 'members))


;(quit)
