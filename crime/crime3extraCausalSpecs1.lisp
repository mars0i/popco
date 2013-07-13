;;; crime3extraCausalSpecs1.lisp
;;; Experiment re effects of adding one-way links to source analogs for
;;; causal propns.  Note some causal connections function as iffs.
;;; Further development might be to make weights be governed by activns
;;; of corresponding propns.

;(make-link 'from to 'weight)

;; for virus analog - with corresponding causal propn in comment

(make-link 'v-ia v-ha '1) ; v-ci->ha
(make-link 'v-ipa v-ia '1) ; v-ipa->ia
(make-link 'v-ica v-ipa '-1) ; v-ia->-spa
;(make-link 'v-ia->-spa v-na '1) ; v-iaspa->na
(make-link 'v-qp v-ipa '-1) ; v-qp->-spa
;(make-link 'v-qp->-spa  v-na '1) ; v-qpspa->na

; (make-link 'cv-ca cv-ha '1) ; cv-ca->hp
; (make-link 'cv-rpa cv-ca '1) ; cv-sca->ca
; (make-link 'cv-sa cv-rpa '-1) ; cv-sa->-rpa
; ;(make-link 'cv-sa->-rpa cv-na '1) ; cv-sarpa->na
; (make-link 'cv-ip cv-rpa '-1) ; cv-ip->-rpa
; ;(make-link 'cv-ip->-rpa  cv-na '1) ; cv-iprpa->na

; (make-link 'b-ab b-abp '1) ; b-ab->abp
; (make-link 'b-abp b-hp '1) ; b-abp->hp
; (make-link 'b-abp b-hb '1) ; b-abp->hb
; (make-link 'b-cpb b-abp '-1) ; b-cpb->-abp
; (make-link 'b-cpb b-dtp '1) ; b-cpb->dtp

; (make-link 'cb-ap cb-vpp '1) ; cb-ap->vpp
; (make-link 'cb-vpp cb-hcp '1) ; cb-vpp->hcp
; (make-link 'cb-vpp cb-hp '1) ; cb-vpp->hp
; (make-link 'cb-cpc cb-vpp '-1) ; cb-cpc->-vpp
; (make-link 'cb-cpc cb-dtp '1) ; cb-cpc->dtp
