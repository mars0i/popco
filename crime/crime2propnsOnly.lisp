;; Thibodeau/Boroditsky crime is a beast/virus metaphors modeled in ACME

(defvar virus-propns
  '(
    (is-infected (prev-infected-elt) v-ip) 
    (not-infected (at-risk-elt) v-na)  
    (is-infected (at-risk-elt) v-ia)   
    (harms (at-risk-elt) v-ha)        
    (cause (v-ia v-ha) v-ci->ha)
    (harms (prev-infected-elt) v-hp)  
    (cause (v-ip v-hp) v-ci->hp)         
    (infect (prev-infected-elt at-risk-elt) v-ipa) 
    (cause (v-ipa v-ia) v-ipa->ia) 
    (innoculate (at-risk-elt) v-ica)   
    (prevent (v-ica v-ipa) v-ia->-spa) 
    (cause (v-ia->-spa v-na) v-iaspa->na) 
    (quarantine (prev-infected-elt) v-qp) 
    (prevent (v-qp v-ipa) v-qp->-spa) 
    (cause (v-qp->-spa  v-na) v-qpspa->na)
    (treat (prev-infected-elt) v-tp)  
    (prevent (v-tp v-ipa) v-tp->-spa) 
    (cause (v-tp->-spa v-na) v-tpspa->na)
   ))

(defvar viral-crime-propns
  '(
    (is-criminal (prev-criminal-cperson) cv-cp) 
    (not-criminal (at-risk-cperson) cv-na)      
    (is-criminal (at-risk-cperson) cv-ca)
    (harms (at-risk-cperson) cv-ha)
    (cause (cv-ca cv-ha) cv-ca->hp)         
    (harms (prev-criminal-cperson) cv-hp)
    (cause (cv-cp cv-hp) cv-cp->hp)         
    (recruit (prev-criminal-cperson at-risk-cperson) cv-rpa) 
    (cause (cv-rpa cv-ca) cv-sca->ca) 
    (support (at-risk-cperson) cv-sa) 
    (prevent (cv-sa cv-rpa) cv-sa->-rpa)
    (cause (cv-sa->-rpa cv-na) cv-sarpa->na)
    (imprison (prev-criminal-cperson) cv-ip)
    (prevent (cv-ip cv-rpa) cv-ip->-rpa)
    (cause (cv-ip->-rpa  cv-na) cv-iprpa->na) 
    (reform (prev-criminal-cperson) cv-rp) 
    (prevent (cv-rp cv-rpa) cv-rp->-rpa)
    (cause (cv-rp->-rpa cv-na) cv-rprpa->na)
   ))



(defvar beastly-crime-propns
  '(
    (not-criminal (cperson) cb-np)   
    (victimize (prev-criminal-cperson cperson) cb-vpp)
    (harms (cperson) cb-hcp) 
    (cause (cb-vpp cb-hcp) cb-vpp->hcp) 
    (helps (prev-criminal-cperson) cb-hp)
    (cause (cb-vpp cb-hp) cb-vpp->hp) 
    (capture (prev-criminal-cperson) cb-cpc) 
    (prevent (cb-cpc cb-vpp) cb-cpc->-vpp)
    (aggressive (prev-criminal-cperson) cb-ap)
   ))

(defvar beast-propns
  '(
    (human (bperson) b-pp)   
    (attack (beast bperson) b-abp)
    (harms (bperson) b-hp)
    (cause (b-abp b-hp) b-abp->hp) 
    (helps (beast) b-hb)
    (cause (b-abp b-hb) b-abp->hb) 
    (capture (beast) b-cpb)
    (prevent (b-cpb b-abp) b-cpb->-abp)
    (aggressive (beast) b-ab)
   ))


(defvar semantic-relations
  '(
    (similar 'cause 'prevent (* -1 *ident-weight*)) 
    (semantic-iff 'cb-vpp 'v-ipa -.1)
    (semantic-iff 'cv-rpa 'b-abp -.1)
   ))
