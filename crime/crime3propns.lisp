;;; crime3propns.lisp
;;; The crime3 version of the "crime is a virus/beast" propositions.
;;; To be loaded by crime3.lisp.
;;; This file has all of the the propositonal and semantic contents of
;;; crime3.lisp as of 2/20/2013, with everything but the propositions and 
;;; semantic specs stripped out.  In addition to things like person-defining
;;; functions, I stripped out candidate propositions that I'd commented out
;;; and have not used for a long time.
;;; See crime3withnotes.lisp for all of the original comments, possible propositions not in use, etc.

; Notational conventions:
; Source analog: V-: virus propn | B-: beast propn 
; Target analog: CV-: virus-ey crime propn | CB-: beast-ey crime propn
; BPERSON: person in the beast/virus domains; CPERSON: person in the crime domains
; ELTS: things subject to viruses or crime in the virusey sense 
; X->Y: X causes Y to occur, where X and Y are propositions
; X->-Y: X prevents Y from occuring, where X and Y are propositions
; HO1: This is a higher-order proposition referencing only first-order propositions
; HO2: This is a higher-order proposition referencing at least one HO1 proposition
; HO3: This is a higher-order proposition referencing at least one HO2 proposition
; etc.

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

(defvar viral-crime-propns
  '(
    (is-criminal (crim-pers) cv-cp) ; person who's already committing crimes
    (not-criminal (at-risk-cpers) cv-na)      ; person at risk of turning to crime
    (is-criminal (at-risk-cpers) cv-ca)
    (harms (at-risk-cpers) cv-ha)
    (cause (cv-ca cv-ha) cv-ca->hp)         ; becoming a criminal has bad consequences for the individual
    (recruit (crim-pers at-risk-cpers) cv-rpa) ; criminals recruit, teach, are role models for, cause indirectly new criminals
    (cause (cv-rpa cv-ca) cv-sca->ca) ; 
    (support (at-risk-cpers) cv-sa) ; support = financial support, supportive parents, mentors, etc., education, etc.
    (prevent (cv-sa cv-rpa) cv-sa->-rpa)     ; [HO1]
    (cause (cv-sa->-rpa cv-na) cv-sarpa->na) ; [HO2]
    (imprison (crim-pers) cv-ip)
    (prevent (cv-ip cv-rpa) cv-ip->-rpa)
    (cause (cv-ip->-rpa  cv-na) cv-iprpa->na) ; imprisoning prevents crime [HO2]
   ))

(defvar beast-propns
  '(
    (human (bpers) b-pp)                  ; person is human [should match: (not-criminal (cpers) cb-np)]
    (aggressive (beast) b-ab)             ; beast is agressive
    (attack (beast bpers) b-abp)          ; beast attacks person
    (cause (b-ab b-abp) b-ab->abp)        ; beast's agressiveness causes it to attack person [HO1]
    (harms (bpers) b-hp)                  ; person is harmed
    (cause (b-abp b-hp) b-abp->hp)        ; beast attacking human harms person [HO1]
    (helps (beast) b-hb)                  ; beast is benefited
    (cause (b-abp b-hb) b-abp->hb)        ; beast attacking person benefits beast [HO1]
    (capture (bpers beast) b-cpb)         ; person captures beast
    (prevent (b-cpb b-abp) b-cpb->-abp)   ; person capturing beast prevents beast attacking person [HO1]
    (danger-to (bpers) b-dtp)             ; person is subject to danger
    (cause (b-cpb b-dtp) b-cpb->dtp)      ; person capturing beast is dangerous to person [HO1]
   ))

(defvar beastly-crime-propns
  '(
    (not-criminal (cpers) cb-np)           ; person is not a crinimal
    (aggressive (crim-pers) cb-ap)         ; person who's already a criminal is aggressive
    (victimize (crim-pers cpers) cb-vpp)   ; criminal victimizes non-criminal
    (cause (cb-ap cb-vpp) cb-ap->vpp)      ; criminal's aggressiveness causes himer to victimize non-criminal [HO1]
    (harms (cpers) cb-hcp)                 ; non-criminal is harmed [hp already in use as name]
    (cause (cb-vpp cb-hcp) cb-vpp->hcp)    ; criminal victimizing non-criminal harms non-criminal [HO1]
    (helps (crim-pers) cb-hp)              ; criminal is benefited
    (cause (cb-vpp cb-hp) cb-vpp->hp)      ; criminal victimizing non-criminal benefits criminal [HO1]
    (capture (cpers crim-pers) cb-cpc)     ; non-criminal captures criminal [cp already in use for crime propn]
    (prevent (cb-cpc cb-vpp) cb-cpc->-vpp) ; non-criminal capturing criminal prevents criminal from victimizing non-criminal [HO1]
    (danger-to (cpers) cb-dtp)             ; non-criminal is subject to danger
    (cause (cb-cpc cb-dtp) cb-cpc->dtp)    ; non-criminal capturing criminal is dangerous to non-criminal [HO1]
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
