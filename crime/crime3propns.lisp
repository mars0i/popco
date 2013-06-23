;;; crime3propns.lisp
;;; The crime3 version of the "crime is a virus/beast" propositions.
;;; To be included by crime3.lisp.
;;; This file has the propositonal and semantic contents of
;;; crime3.lisp as of 2/20/2013, with everything but the
;;; propositions and semantic specs stripped out.
;;; For possible embedding in presentations and papers.
;;; Comments add/altered after 6/22/2013.
;;; See crime3withnotes.lisp for all of the original comments,
;;; possible propositions not in use, etc.

; Notational conventions:
; v-: virus | c-: crime | cv-: virus-ey crime | cb-: beast-ey crime | b-: beast
; X->Y: X causes Y to occur, where X and Y are propositions
; X->-Y: X prevents Y from occuring, where X and Y are propositions
; bperson: person in the beast/virus domain; ; cperson: person in the crime domain
; elts: things subject to viruses or crime in the virusey sense 

(defvar virus-propns
  '(
    (is-infected (prev-infected-elt) v-ip) ; the existence of a previously infected element is assumed
    (not-infected (at-risk-elt) v-na)  ; at risk element is not infected
    (is-infected (at-risk-elt) v-ia)   ; at-risk person/thing is infected
    (harms (at-risk-elt) v-ha)        ; note we have no quantificatiion or true pattern matching
    (cause (v-ia v-ha) v-ci->ha)
    (infect (prev-infected-elt at-risk-elt) v-ipa) ; infection spreads from the previously infected to the at-risk
    (cause (v-ipa v-ia) v-ipa->ia) ; transmission from infected to uninfected causes infection
    (inoculate (at-risk-elt) v-ica)   ; inoculating the at-risk prevents spread to new individuals (or something with cells?)
    (prevent (v-ica v-ipa) v-ia->-spa) ; inoculation of uninfected prevents further infection
    (cause (v-ia->-spa v-na) v-iaspa->na) ; preventing spread of infection causes [preserves] lack of infection in the at-risk
    (quarantine (prev-infected-elt) v-qp) ; previously infected is quarantined (or cells sequestered, I suppose)
    (prevent (v-qp v-ipa) v-qp->-spa) ; quarantining the infected prevents spread to new individuals 
    (cause (v-qp->-spa  v-na) v-qpspa->na)
   ))

(defvar beast-propns
  '(
    (human (bperson) b-pp)   ; supposed to match: (not-criminal (cperson) cb-np)
    (aggressive (beast) b-ab)
    (attack (beast bperson) b-abp)
    (cause (b-ab b-abp) b-ab->abp)
    (harms (bperson) b-hp)
    (cause (b-abp b-hp) b-abp->hp) ; being attacked is harmful
    (helps (beast) b-hb)
    (cause (b-abp b-hb) b-abp->hb) ; beasts benefit from attacking--e.g get food
    (capture (bperson beast) b-cpb)
    (prevent (b-cpb b-abp) b-cpb->-abp)
    (danger-to (bperson) b-dtp)
    (cause (b-cpb b-dtp) b-cpb->dtp)
   ))

(defvar viral-crime-propns
  '(
    (is-criminal (prev-criminal-cperson) cv-cp) ; person who's already committing crimes
    (not-criminal (at-risk-cperson) cv-na)      ; person at risk of turning to crime
    (is-criminal (at-risk-cperson) cv-ca)
    (harms (at-risk-cperson) cv-ha)
    (cause (cv-ca cv-ha) cv-ca->hp)         ; becoming a criminal has bad consequences for the individual
    (recruit (prev-criminal-cperson at-risk-cperson) cv-rpa) ; criminals recruit, teach, are role models for, cause indirectly new criminals
    (cause (cv-rpa cv-ca) cv-sca->ca) ; 
    (support (at-risk-cperson) cv-sa) ; support = financial support, supportive parents, mentors, etc., education, etc.
    (prevent (cv-sa cv-rpa) cv-sa->-rpa)
    (cause (cv-sa->-rpa cv-na) cv-sarpa->na)
    (imprison (prev-criminal-cperson) cv-ip)
    (prevent (cv-ip cv-rpa) cv-ip->-rpa)
    (cause (cv-ip->-rpa  cv-na) cv-iprpa->na) ; imprisoning prevents crime
   ))

(defvar beastly-crime-propns
  '(
    (not-criminal (cperson) cb-np)   ; person not at risk of turning to crime
    (aggressive (prev-criminal-cperson) cb-ap)
    (victimize (prev-criminal-cperson cperson) cb-vpp)
    (cause (cb-ap cb-vpp) cb-ap->vpp)
    (harms (cperson) cb-hcp) ; hp already in use as a name
    (cause (cb-vpp cb-hcp) cb-vpp->hcp) ; existing criminals harm those not at risk
    (helps (prev-criminal-cperson) cb-hp)
    (cause (cb-vpp cb-hp) cb-vpp->hp) ; criminals benefit from attacking--e.g get money
    (capture (cperson prev-criminal-cperson) cb-cpc) ; cp is already used as name for crime propn
    (prevent (cb-cpc cb-vpp) cb-cpc->-vpp)
    (danger-to (cperson) cb-dtp)
    (cause (cb-cpc cb-dtp) cb-cpc->dtp)
   ))

(defvar semantic-relations
  '(
    (similar 'cause 'prevent (* -1 *ident-weight*)) ; avoid mapping cause to prevent
    (semantic-iff 'cb-vpp 'v-ipa -.1)
    (semantic-iff 'cv-rpa 'b-abp -.1)
    (similar 'is-beastly 'is-infected (* -1 *ident-weight*))
   ))

(defvar crime-propns `(,@viral-crime-propns ,@beastly-crime-propns))

(defvar virus-propn-syms (mapcar #'third virus-propns))
(defvar beast-propn-syms (mapcar #'third beast-propns))
(defvar viral-crime-propn-syms (mapcar #'third viral-crime-propns))
(defvar beastly-crime-propn-syms (mapcar #'third beastly-crime-propns))
(defvar crime-propn-syms (mapcar #'third crime-propns))
