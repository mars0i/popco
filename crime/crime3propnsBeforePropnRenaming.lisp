;;; crime3propns.lisp
;;; The crime3 version of the "crime is a virus/beast" propositions.
;;; To be loaded by crime3.lisp.
;;; NEW 7/14/2013:
;;; Renamed CAUSE and PREVENT predicates to CAUSAL-IF[F] and PREVENTATIVE-IF[F]
;;; for use with update-propn-nets-from-propn-nets and modified make-propn.

;;; This file has all of the the propositonal and semantic contents of
;;; crime3.lisp as of 2/20/2013, with everything but the propositions and 
;;; semantic specs stripped out.  In addition to things like person-defining
;;; functions, I stripped out candidate propositions that I'd commented out
;;; and have not used for a long time.

;;; See crime3withnotes.lisp for all of the original comments, possible propositions not in use, etc.
;;; Note that the virus and crime prevention propositions are especially approximate/abstracted,
;;; partly because we have no quantifiers.
;;; crime4propns.lisp

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
    (is-infected (vpers0) v-ip)                ; 0. Person 0 has infection.
    (not-infected (vpers1) v-na)               ; 1. Person 1 lacks infection.
    (is-infected (vpers1) v-ia)                ; 2. Person 1 has infection.
    (harms (vpers1) v-ha)                      ; 3. Person 1 is harmed. [PREVENTING THIS IS GOAL.]
    (CAUSAL-IF (v-ia v-ha) v-ia->ha)           ; 4. That person 1 has infection is harmful to person 1. [HO1]
    (infect (vpers0 VPERS1) v-ipa)             ; 5. Person 0, who already has infection, infects person 1.
    (CAUSAL-IF (v-ipa v-ia) v-ipa->ia)         ; 6. The infecting of person 1 by person 0 causes person 1 to have infection. [HO1]
    (inoculate (vpers1) v-ica)                 ; 7. Person 1 gets innoculated.
    (PREVENTATIVE-IF (v-ica v-ipa) v-ia->-spa) ; 8. That person 1 is innoculated prevents person 0 from infecting person 1. [HO1]
    (CAUSAL-IF (v-ia->-spa v-na) v-iaspa->na)  ; 9. That the innoculating prevents the infecting causes [preserves] person 1 lacking infection. [HO2]
    (quarantine (vpers0) v-qp)                 ; 10. Person 0 is quarantined.
    (PREVENTATIVE-IF (v-qp v-ipa) v-qp->-spa)  ; 11. That person 0 is quarantined prevents person 0 from infecting person 1.
    (CAUSAL-IF (v-qp->-spa  v-na) v-qpspa->na) ; 12. That (quarantining 0 prevents 0 from infecting 1) causes [preserves] person 1 lacking infection. [HO2]
   ))

(defvar viral-crime-propns
  '(
    (is-criminal (cpers0) cv-cp)                  ; 0. Person 0 is a criminal.
    (not-criminal (cpers1) cv-na)                 ; 1. Person 1 is not a criminal (or: is innocent).
    (is-criminal (cpers1) cv-ca)                  ; 2. Person 1 is a criminal.
    (harms (cpers1) cv-ha)                        ; 3. Person 1 is harmed. [PREVENTING THIS IS GOAL.]
    (CAUSAL-IF (cv-ca cv-ha) cv-ca->ha)           ; 4. Person 1 being a criminal is harmful to person 1.
    (recruit (cpers0 cpers1) cv-rpa)              ; 5. Person 0 recruits person 1 into crime.
    (CAUSAL-IF (cv-rpa cv-ca) cv-rpa->ca)         ; 6. Person 0 recruiting person 1 causes person 1 to become a criminal. [HO1]
    (support (cpers1) cv-sa)                      ; 7. Person 1 is [financially, parentally, socially, educationally, etc.] supported.
    (PREVENTATIVE-IF (cv-sa cv-rpa) cv-sa->-rpa)  ; 8. Person 1 being supported prevents person 0 from recruiting person 1. [HO1]
    (CAUSAL-IF (cv-sa->-rpa cv-na) cv-sarpa->na)  ; 9. That being supported prevents 1 from being recruited by 0 causes [preserves] 1's innocence. [HO2]
    (imprison (cpers0) cv-ip)                     ; 10. Person 0 is imprisoned.  [Alternative: is reformed]
    (PREVENTATIVE-IF (cv-ip cv-rpa) cv-ip->-rpa)  ; 11. Person 0 being imprisoned prevents person 0 from recruiting person 1. [HO1]
    (CAUSAL-IF (cv-ip->-rpa  cv-na) cv-iprpa->na) ; 12. That O's imprisonment prevents 0 from recruiting 1 causes [preserves] 1's innocence. [HO2]
   ))

(defvar beast-propns
  '(
    (human (bpers) b-pp)                        ; 0. Person is human. [should match cb-np]
    (aggressive (beast) b-ab)                   ; 1. Beast is agressive.
    (attack (beast bpers) b-abp)                ; 2. Beast attacks person.
    (CAUSAL-IF (b-ab b-abp) b-ab->abp)          ; 3. Beast's agressiveness causes it to attack person. [HO1]
    (harms (bpers) b-hp)                        ; 4. Person is harmed. [PREVENTING THIS IS GOAL.]
    (CAUSAL-IF (b-abp b-hp) b-abp->hp)          ; 5. Beast attacking human harms person. [HO1]
    (helps (beast) b-hb)                        ; 6. Beast is benefited.
    (CAUSAL-IF (b-abp b-hb) b-abp->hb)          ; 7. Beast attacking person benefits beast. [HO1]
    (capture (bpers beast) b-cpb)               ; 8. Person captures beast.
    (PREVENTATIVE-IF (b-cpb b-abp) b-cpb->-abp) ; 9. Person capturing beast prevents beast attacking person. [HO1]
    (danger-to (bpers) b-dtp)                   ; 10. Person is subject to danger.
    (CAUSAL-IF (b-cpb b-dtp) b-cpb->dtp)        ; 11. Person capturing beast is dangerous to person. [HO1]
   ))

(defvar beastly-crime-propns
  '(
    (not-criminal (cpers) cb-np)                   ; 0. Person is not a crinimal.
    (aggressive (crim-pers) cb-ap)                 ; 1. Person who's already a criminal is aggressive.
    (victimize (crim-pers cpers) cb-vpp)           ; 2. Criminal victimizes non-criminal.
    (CAUSAL-IF (cb-ap cb-vpp) cb-ap->vpp)          ; 3. Criminal's aggressiveness causes himer to victimize non-criminal. [HO1]
    (harms (cpers) cb-hcp)                         ; 4. Non-criminal is harmed. [hp already in use as name] [PREVENTING THIS IS GOAL.]
    (CAUSAL-IF (cb-vpp cb-hcp) cb-vpp->hcp)        ; 5. Criminal victimizing non-criminal harms non-criminal. [HO1]
    (helps (crim-pers) cb-hp)                      ; 6. Criminal is benefited.
    (CAUSAL-IF (cb-vpp cb-hp) cb-vpp->hp)          ; 7. Criminal victimizing non-criminal benefits criminal. [HO1]
    (capture (cpers crim-pers) cb-cpc)             ; 8. Non-criminal captures criminal. [notation "cp" has another use]
    (PREVENTATIVE-IF (cb-cpc cb-vpp) cb-cpc->-vpp) ; 9. Non-criminal capturing criminal prevents criminal from victimizing non-criminal. [HO1]
    (danger-to (cpers) cb-dtp)                     ; 10. Non-criminal is subject to danger.
    (CAUSAL-IF (cb-cpc cb-dtp) cb-cpc->dtp)        ; 11. Non-criminal capturing criminal is dangerous to non-criminal. [HO1]
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
