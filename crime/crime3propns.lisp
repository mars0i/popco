;;; crime3propns.lisp
;;; The crime3 version of the "crime is a virus/beast" propositions.
;;; To be loaded by crime3.lisp.
;;; NEW 7/14/2013:
;;; Renamed CAUSE and PREVENT predicates to CAUSAL-IF[F] and PREVENTATIVE-IF[F]
;;; for use with update-propn-nets-from-propn-nets and modified make-propn.
;;; NEW 7/21/2013:
;;; RENAMED CAUSAL PROPNS so they completely consistently and precisely reflect
;;; their arguments.  This is a bit odd when an argument is causal.
;;; This will allow finding the causal propns from pairs of potential arguments.
;;; NOTE POTENTIAL BUG in unusual circumstances. e.g. these would have the same name
;;; under this scheme:
;;; (causal-if (yo->ya hey) yo->ya->hey)
;;; (causal-if (yo ya->hey) yo->ya->hey)
;;; And as before, causal relations are represented in names by "->", and preventative
;;; by "->-".  Maintaining these conventions will allow construction of useful GUESS
;;; representations.
;;; 
;;; ALSO I discovered a bug with propns clobbering others:
;;; Used to say:
;;;  (harms (bpers) B-HP)                        ; 4. Person is harmed. [PREVENTING THIS IS GOAL.]
;;;  (helps (beast) B-HB)                        ; 6. Beast is benefited.
;;;  (causal-if (b-abp b-hp) B-ABP->HP)          ; 5. Beast attacking human harms person. [HO1]
;;;  (causal-if (b-abp b-hb) B-ABP->HB)          ; 7. Beast attacking person benefits beast. [HO1]
;;; Fixed for the first time now 7/21/2013.


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
    (CAUSAL-IF (v-ia v-ha) v-ia->v-ha)           ; 4. That person 1 has infection is harmful to person 1. [HO1]
    (infect (vpers0 vpers1) v-ipa)             ; 5. Person 0, who already has infection, infects person 1.
    (CAUSAL-IF (v-ipa v-ia) v-ipa->v-ia)         ; 6. The infecting of person 1 by person 0 causes person 1 to have infection. [HO1]
    (inoculate (vpers1) v-ica)                 ; 7. Person 1 gets innoculated.
    (PREVENTATIVE-IF (v-ica v-ipa) v-ica->-v-ipa) ; 8. That person 1 is innoculated prevents person 0 from infecting person 1. [HO1]
    (CAUSAL-IF (v-ica->-v-ipa v-na) v-ica->-v-ipa->v-na)  ; 9. That the innoculating prevents the infecting causes [preserves] person 1 lacking infection. [HO2]
    (quarantine (vpers0) v-qp)                 ; 10. Person 0 is quarantined.
    (PREVENTATIVE-IF (v-qp v-ipa) v-qp->-v-ipa)  ; 11. That person 0 is quarantined prevents person 0 from infecting person 1.
    (CAUSAL-IF (v-qp->-v-ipa v-na) v-qp->-v-ipa->v-na) ; 12. That (quarantining 0 prevents 0 from infecting 1) causes [preserves] person 1 lacking infection. [HO2]
   ))

(defvar viral-crime-propns
  '(
    (is-criminal (cpers0) cv-cp)                  ; 0. Person 0 is a criminal.
    (not-criminal (cpers1) cv-na)                 ; 1. Person 1 is not a criminal (or: is innocent).
    (is-criminal (cpers1) cv-ca)                  ; 2. Person 1 is a criminal.
    (harms (cpers1) cv-ha)                        ; 3. Person 1 is harmed. [PREVENTING THIS IS GOAL.]
    (CAUSAL-IF (cv-ca cv-ha) cv-ca->cv-ha)           ; 4. Person 1 being a criminal is harmful to person 1.
    (recruit (cpers0 cpers1) cv-rpa)              ; 5. Person 0 recruits person 1 into crime.
    (CAUSAL-IF (cv-rpa cv-ca) cv-rpa->cv-ca)         ; 6. Person 0 recruiting person 1 causes person 1 to become a criminal. [HO1]
    (support (cpers1) cv-sa)                      ; 7. Person 1 is [financially, parentally, socially, educationally, etc.] supported.
    (PREVENTATIVE-IF (cv-sa cv-rpa) cv-sa->-cv-rpa)  ; 8. Person 1 being supported prevents person 0 from recruiting person 1. [HO1]
    (CAUSAL-IF (cv-sa->-cv-rpa cv-na) cv-sa->-cv-rpa->cv-na)  ; 9. That being supported prevents 1 from being recruited by 0 causes [preserves] 1's innocence. [HO2]
    (imprison (cpers0) cv-ip)                     ; 10. Person 0 is imprisoned.  [Alternative: is reformed]
    (PREVENTATIVE-IF (cv-ip cv-rpa) cv-ip->-cv-rpa)  ; 11. Person 0 being imprisoned prevents person 0 from recruiting person 1. [HO1]
    (CAUSAL-IF (cv-ip->-cv-rpa  cv-na) cv-ip->-cv-rpa->cv-na) ; 12. That O's imprisonment prevents 0 from recruiting 1 causes [preserves] 1's innocence. [HO2]
   ))

(defvar beast-propns
  '(
    (human (bpers) b-pp)                        ; 0. Person is human. [should match cb-np]
    (aggressive (beast) b-ab)                   ; 1. Beast is agressive.
    (attack (beast bpers) b-abp)                ; 2. Beast attacks person.
    (CAUSAL-IF (b-ab b-abp) b-ab->b-abp)          ; 3. Beast's agressiveness causes it to attack person. [HO1]
    (harms (bpers) b-hrp)                        ; 4. Person is harmed. [PREVENTING THIS IS GOAL.]
    (CAUSAL-IF (b-abp b-hrp) b-abp->b-hrp)          ; 5. Beast attacking human harms person. [HO1]
    (helps (beast) b-hlb)                        ; 6. Beast is benefited.
    (CAUSAL-IF (b-abp b-hlb) b-abp->b-hlb)          ; 7. Beast attacking person benefits beast. [HO1]
    (capture (bpers beast) b-cpb)               ; 8. Person captures beast.
    (PREVENTATIVE-IF (b-cpb b-abp) b-cpb->-b-abp) ; 9. Person capturing beast prevents beast attacking person. [HO1]
    (danger-to (bpers) b-dtp)                   ; 10. Person is subject to danger.
    (CAUSAL-IF (b-cpb b-dtp) b-cpb->b-dtp)        ; 11. Person capturing beast is dangerous to person. [HO1]
   ))

(defvar beastly-crime-propns
  '(
    (not-criminal (cpers) cb-np)                   ; 0. Person is not a crinimal.
    (aggressive (crim-pers) cb-ap)                 ; 1. Person who's already a criminal is aggressive.
    (victimize (crim-pers cpers) cb-vpp)           ; 2. Criminal victimizes non-criminal.
    (CAUSAL-IF (cb-ap cb-vpp) cb-ap->cb-vpp)          ; 3. Criminal's aggressiveness causes himer to victimize non-criminal. [HO1]
    (harms (cpers) cb-hrp)                         ; 4. Non-criminal is harmed. [PREVENTING THIS IS GOAL.]
    (CAUSAL-IF (cb-vpp cb-hrp) cb-vpp->cb-hrp)        ; 5. Criminal victimizing non-criminal harms non-criminal. [HO1]
    (helps (crim-pers) cb-hlp)                      ; 6. Criminal is benefited.
    (CAUSAL-IF (cb-vpp cb-hlp) cb-vpp->cb-hlp)          ; 7. Criminal victimizing non-criminal benefits criminal. [HO1]
    (capture (cpers crim-pers) cb-cpc)             ; 8. Non-criminal captures criminal. [notation "cp" has another use]
    (PREVENTATIVE-IF (cb-cpc cb-vpp) cb-cpc->-cb-vpp) ; 9. Non-criminal capturing criminal prevents criminal from victimizing non-criminal. [HO1]
    (danger-to (cpers) cb-dtp)                     ; 10. Non-criminal is subject to danger.
    (CAUSAL-IF (cb-cpc cb-dtp) cb-cpc->cb-dtp)        ; 11. Non-criminal capturing criminal is dangerous to non-criminal. [HO1]
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
