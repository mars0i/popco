;;; crime3propns.lisp
;;; The crime3 version of the "crime is a virus/beast" propositions.
;;; To be loaded by crime3.lisp.
;;; This file has all of the the propositonal and semantic contents of
;;; crime3.lisp as of 2/20/2013, with everything but the propositions and 
;;; semantic specs stripped out.  In addition to things like person-defining
;;; functions, I stripped out candidate propositions that I'd commented out
;;; and have not used for a long time.
;;; See crime3withnotes.lisp for all of the original comments, possible propositions not in use, etc.
;;; Note that the virus and crime prevention propositions are especially approximate/abstracted,
;;; partly because we have no quantifiers.

; Notational conventions:
; Source analog: V-: virus propn | B-: beast propn 
; Target analog: CV-: virus-ey crime propn | CB-: beast-ey crime propn
; BPERS: person in the beast domain; CPERS: person in the crime domains.
; VPERS-0: person who's already infected | VPERS-1: initially uninfected person who might get infected.
; CPERS-0: person who's already a criminal | CPERS-1: initially innocent person who might become criminal.
; X->Y: X causes Y to occur, where X and Y are propositions
; X->-Y: X prevents Y from occuring, where X and Y are propositions
; HO1: This is a higher-order proposition referencing only first-order propositions
; HO2: This is a higher-order proposition referencing at least one HO1 proposition
; HO3: This is a higher-order proposition referencing at least one HO2 proposition
; etc.

; NOTE
; There's a lot of similarity between quarantine/imprison and capture/capture.
; Also note that Thibodeau and Boroditsky counted arresting, imprisoning, etc.
; as enforcement--i.e. what beast evokes.  Maybe a better analog of quarantining
; would be keeping out of your neighborhood, living in a gated community, etc.
; although you wouldn't necessarily expect those to correlate with prevention
; measures.

(defvar virus-propns
  '(
    (is-infected (vpers-0) v-ip)              ; Person 0 has infection.
    (not-infected (vpers-1) v-na)             ; Person 1 lacks infection.
    (is-infected (vpers-1) v-ia)              ; Person 1 has infection.
    (harms (vpers-1) v-ha)                    ; Person 1 is harmed.
    (cause (v-ia v-ha) v-ci->ha)              ; That person 1 has infection is harmful to person 1. [HO1]
    (infect (vpers-0 pers-1) v-ipa)           ; Person 0, who already has infection, infects person 1.
    (cause (v-ipa v-ia) v-ipa->ia)            ; The infecting of person 1 by person 0 causes person 1 to have infection. [HO1]
    (inoculate (vpers-1) v-ica)               ; Person 1 gets innoculated.
    (prevent (v-ica v-ipa) v-ia->-spa)        ; That person 1 is innoculated prevents person 0 from infecting person 1. [HO1]
    (cause (v-ia->-spa v-na) v-iaspa->na)     ; That the innoculating prevents the infecting causes [preserves] person 1 lacking infection. [HO2]
    (quarantine (vpers-0) v-qp)               ; Person 0 is quarantined.
    (prevent (v-qp v-ipa) v-qp->-spa)         ; That person 0 is quarantined prevents person 0 from infecting person 1.
    (cause (v-qp->-spa  v-na) v-qpspa->na)    ; That (quarantining 0 prevents 0 from infecting 1) causes [preserves] person 1 lacking infection. [HO2]
   ))

(defvar viral-crime-propns
  '(
    (is-criminal (cpers-0) cv-cp)             ; Person 0 is a criminal.
    (not-criminal (cpers-1) cv-na)            ; Person 1 is not a criminal (or: is innocent).
    (is-criminal (cpers-1) cv-ca)             ; Person 1 is a criminal.
    (harms (cpers-1) cv-ha)                   ; Person 1 is harmed.
    (cause (cv-ca cv-ha) cv-ca->hp)           ; Person 1 being a criminal is harmful to person 1.
    (recruit (cpers-0 cpers-1) cv-rpa)        ; Person 0 recruits person 1 into crime.
    (cause (cv-rpa cv-ca) cv-sca->ca)         ; Person 0 recruiting person 1 causes person 1 to become a criminal. [HO1]
    (support (cpers-1) cv-sa)                 ; Person 1 is [financially, parentally, socially, educationally, etc.] supported.
    (prevent (cv-sa cv-rpa) cv-sa->-rpa)      ; Person 1 being supported prevents person 0 from recruiting person 1. [HO1]
    (cause (cv-sa->-rpa cv-na) cv-sarpa->na)  ; That being supported prevents 1 from being recruited by 0 causes [preserves] 1's innocence. [HO2]
    (imprison (cpers-0) cv-ip)                ; Person 0 is imprisoned.  [Alternative: is reformed]
    (prevent (cv-ip cv-rpa) cv-ip->-rpa)      ; Person 0 being imprisoned prevents person 0 from recruiting person 1. [HO1]
    (cause (cv-ip->-rpa  cv-na) cv-iprpa->na) ; That O's imprisonment prevents 0 from recruiting 1 causes [preserves] 1's innocence. [HO2]
   ))

(defvar beast-propns
  '(
    (human (bpers) b-pp)                  ; Person is human. [should match cb-np]
    (aggressive (beast) b-ab)             ; Beast is agressive.
    (attack (beast bpers) b-abp)          ; Beast attacks person.
    (cause (b-ab b-abp) b-ab->abp)        ; Beast's agressiveness causes it to attack person. [HO1]
    (harms (bpers) b-hp)                  ; Person is harmed.
    (cause (b-abp b-hp) b-abp->hp)        ; Beast attacking human harms person. [HO1]
    (helps (beast) b-hb)                  ; Beast is benefited.
    (cause (b-abp b-hb) b-abp->hb)        ; Beast attacking person benefits beast. [HO1]
    (capture (bpers beast) b-cpb)         ; Person captures beast.
    (prevent (b-cpb b-abp) b-cpb->-abp)   ; Person capturing beast prevents beast attacking person. [HO1]
    (danger-to (bpers) b-dtp)             ; Person is subject to danger.
    (cause (b-cpb b-dtp) b-cpb->dtp)      ; Person capturing beast is dangerous to person. [HO1]
   ))

(defvar beastly-crime-propns
  '(
    (not-criminal (cpers) cb-np)           ; Person is not a crinimal.
    (aggressive (crim-pers) cb-ap)         ; Person who's already a criminal is aggressive.
    (victimize (crim-pers cpers) cb-vpp)   ; Criminal victimizes non-criminal.
    (cause (cb-ap cb-vpp) cb-ap->vpp)      ; Criminal's aggressiveness causes himer to victimize non-criminal. [HO1]
    (harms (cpers) cb-hcp)                 ; Non-criminal is harmed. [hp already in use as name]
    (cause (cb-vpp cb-hcp) cb-vpp->hcp)    ; Criminal victimizing non-criminal harms non-criminal. [HO1]
    (helps (crim-pers) cb-hp)              ; Criminal is benefited.
    (cause (cb-vpp cb-hp) cb-vpp->hp)      ; Criminal victimizing non-criminal benefits criminal. [HO1]
    (capture (cpers crim-pers) cb-cpc)     ; Non-criminal captures criminal. [notation "cp" has another use]
    (prevent (cb-cpc cb-vpp) cb-cpc->-vpp) ; Non-criminal capturing criminal prevents criminal from victimizing non-criminal. [HO1]
    (danger-to (cpers) cb-dtp)             ; Non-criminal is subject to danger.
    (cause (cb-cpc cb-dtp) cb-cpc->dtp)    ; Non-criminal capturing criminal is dangerous to non-criminal. [HO1]
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
