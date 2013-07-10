;;; crime3extraCausalSpecs1.lisp
;;; Experiment re effects of adding one-way links to source analogs for
;;; causal propns.  Note some causal connections function as iffs.
;;; Further development might be to make weights be governed by activns
;;; of corresponding propns.

;(make-link from to weight)

;; for virus analog
                            ; CORRESPONDING CAUSAL PROPN
(make-link v-ia v-ha 1)     ; v-ci->ha
(make-link v-ipa v-ia 1)    ; v-ipa->ia
(make-link v-ica v-ipa -1)  ; v-ia->-spa
                            ; v-iaspa->na
(make-link v-qp v-ipa -1)   ; v-qp->-spa
                            ; v-qpspa->na
