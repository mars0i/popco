;; experiment

(load "nosettle")      ; don't allow networks to settle, ever, since subtle differences are amplified by communication
(load "crime/crime3")

(setf *do-report-to-netlogo* nil)

(defvar *group-size* 40)

; don't move graph around in telguess:
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")
(setf *do-converse* t)

(make-virus-bias-crime-talker 'jo viral-crime-propns)

(init-pop)
(print (get 'folks 'members))

(make-link 'jo_v-ia 'jo_v-ha 1) ; v-ci->ha
(make-link 'jo_v-ipa 'jo_v-ia 1) ; v-ipa->ia
(make-link 'jo_v-ica 'jo_v-ipa -1) ; v-ia->-spa
;(make-link 'jo_v-ia->-spa 'jo_v-na 1) ; v-iaspa->na
(make-link 'jo_v-qp 'jo_v-ipa -1) ; v-qp->-spa
;(make-link 'jo_v-qp->-spa 'jo_v-na 1) ; v-qpspa->na

(setf *max-pop-ticks* 300)
;(popco)
;(quit)
