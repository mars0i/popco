;; crime2.lisp
;; Thibodeau/Boroditsky-inspired crime is a virus/beast analogies.
;; First version of crime1.lisp was identical to last version of 
;; criment6.lisp.  See that file, criment6.nts for comments and 
;; discussion of choices below.  See also criment6.pdf.

; QUESTION: SHOULD CAUSAL AND PREVENTATIVE CRIME PROPNS BE PERCEIVED?
; OR ONLY THE SIMPLE CRIME PROPNS?

; Notational conventions:
; v-: virus
; c-: crime
; cv-: virus-ey crime
; cb-: beast-ey crime
; b-: beast
; X->Y: X causes Y to occur, where X and Y are propositions
; X->-Y: X prevents Y from occuring, where X and Y are propositions

; To distinguish persons in the beast propositions and persons in the crime propositions,
; I'm calling them bpersons and cpersons, but treating their initial character
; as "p" when constructing abbreviated proposition names.  I don't think there
; would be any problem if there is the same object name in two domains, but
; it's clearer to give them different names.  I don't do the same
; thing in the virus propositions because there I call the things "elements",
; or rather "elts", since they could be e.g. cells rather than persons.

(setf *propn-category-prefixes* '("CV" "CB" "V" "B"))
(setf *propn-category-descriptions* '("virus-like crime propns" "beast-like crime propns" "virus propns" "beast propns")) ; these should match

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar semantic-relations
  '(
    (similar 'cause 'prevent (* -1 *ident-weight*)) ; avoid mapping cause to prevent
    ; it's not getting mapped, anywa, so this isn't doing anything

    ; THESE ARE GETTING CREATED AND THEN ERASED AFTER A TICK OR TWO:
    (semantic-iff 'cb-vpp 'v-ipa -.1)
    (semantic-iff 'cv-rpa 'b-abp -.1)

    ;(similar 'infect 'victimize (* -.5 *ident-weight*))
    ;(similar 'attack 'recruit   (* -.5 *ident-weight*))

    ; These won't do anything because they don't cross analog structures:
    ;(similar 'is-criminal 'not-criminal (* -1 *ident-weight*))
    ;(similar 'is-infected 'not-infected (* -1 *ident-weight*)) ; WONT DO ANYTHING NOT CROSS-DOMAIN

    ; But these can work in their stead:
    ;(semantic-iff 'cv-ca 'cv-na -1.0) ; at-risk-cperson being infected and being uninfected are inconsistent [-1 too strong?]
    ;(semantic-iff 'v-ia 'v-na -1.0) ; at-risk-cperson being infected and being uninfected are inconsistent [-1 too strong?]
   ))

; Note we call the things that get infected "elts", i.e. elements,
; since they could either be people or cells or something like that.
; Also see note about naming person objects the same elsewhere in this file.

(defvar virus-propns
  '(
    (is-infected (prev-infected-elt) v-ip) ; the existence of a previously infected element is assumed
    (not-infected (at-risk-elt) v-na)  ; at risk element is not infected
    (is-infected (at-risk-elt) v-ia)   ; at-risk person/thing is infected

    (harms (at-risk-elt) v-ha)        ; note we have no quantificatiion or true pattern matching
    (cause (v-ia v-ha) v-ci->ha)

    (harms (prev-infected-elt) v-hp)  ; drop? introduces noise?
    (cause (v-ip v-hp) v-ci->hp)         ; drop? introduces noise?

    (infect (prev-infected-elt at-risk-elt) v-ipa) ; infection spreads from the previously infected to the at-risk
    (cause (v-ipa v-ia) v-ipa->ia) ; transmission from infected to uninfected causes infection

    ; The following triplets are a bit awkward and convoluted because we don't have time indexing:

    (innoculate (at-risk-elt) v-ica)   ; innoculating the at-risk prevents spread to new individuals (or something with cells?)
    (prevent (v-ica v-ipa) v-ia->-spa) ; innoculation of uninfected prevents further infection
    (cause (v-ia->-spa v-na) v-iaspa->na) ; preventing spread of infection causes [preserves] lack of infection in the at-risk

    ; Next two triplets are structurally identical. Maybe drop one.

    (quarantine (prev-infected-elt) v-qp) ; previously infected is quarantined (or cells sequestered, I suppose)
    (prevent (v-qp v-ipa) v-qp->-spa) ; quarantining the infected prevents spread to new individuals 
    (cause (v-qp->-spa  v-na) v-qpspa->na)

    (treat (prev-infected-elt) v-tp)  ; treatment of the infected to remove disease prevents later spread
    (prevent (v-tp v-ipa) v-tp->-spa) ; treatment of infected prevents further infection
    (cause (v-tp->-spa v-na) v-tpspa->na)
    ; [We elide the step in which the infected becomes uninfected, which would require time-indexing.]
   ))

(defvar viral-crime-propns
  '(
    (is-criminal (prev-criminal-cperson) cv-cp) ; person who's already committing crimes
    (not-criminal (at-risk-cperson) cv-na)      ; person at risk of turning to crime
    (is-criminal (at-risk-cperson) cv-ca)

    (harms (at-risk-cperson) cv-ha)
    (cause (cv-ca cv-ha) cv-ca->hp)         ; becoming a criminal has bad consequences for the individual

    (harms (prev-criminal-cperson) cv-hp)
    (cause (cv-cp cv-hp) cv-cp->hp)         ; being a criminal has bad consequences for the criminal

    (recruit (prev-criminal-cperson at-risk-cperson) cv-rpa) ; criminals recruit, teach, are role models for, cause indirectly new criminals
    (cause (cv-rpa cv-ca) cv-sca->ca) ; 

    ; The following triplets are a bit awkward and convoluted because we don't have time indexing:

    (support (at-risk-cperson) cv-sa) ; support = financial support, supportive parents, mentors, etc., education, etc.
    (prevent (cv-sa cv-rpa) cv-sa->-rpa)
    (cause (cv-sa->-rpa cv-na) cv-sarpa->na)

    ; Next two triplets are structurally identical. Maybe drop one.

    (imprison (prev-criminal-cperson) cv-ip)
    (prevent (cv-ip cv-rpa) cv-ip->-rpa)
    (cause (cv-ip->-rpa  cv-na) cv-iprpa->na) ; imprisoning prevents crime

    (reform (prev-criminal-cperson) cv-rp) ; reform includes social support, education, etc. to criminals
    (prevent (cv-rp cv-rpa) cv-rp->-rpa)
    (cause (cv-rp->-rpa cv-na) cv-rprpa->na)
   ))

(defvar beastly-crime-propns
  '(
    (not-criminal (cperson) cb-np)   ; person not at risk of turning to crime
    (victimize (prev-criminal-cperson cperson) cb-vpp)
    (harms (cperson) cb-hcp) ; hp already in use as a name
    (cause (cb-vpp cb-hcp) cb-vpp->hcp) ; existing criminals harm those not at risk
    ; added 11/8/12:
    (helps (prev-criminal-cperson) cb-hp)
    (cause (cb-vpp cb-hp) cb-vpp->hp) ; criminals benefit from attacking--e.g get money

    (capture (prev-criminal-cperson) cb-cpc) ; cp is already used as name for crime propn
    (prevent (cb-cpc cb-vpp) cb-cpc->-vpp)
    ; adding any of these doesn't make much difference:
    ;(PREVENT (CB-CPC CB-HCP) CB-CPC->-HCP) ; snap the transitive pointers
    ;(kill (prev-criminal) cb-kp)
    ;(prevent (cb-kp cb-vpp) cb-kb->-vpp)

    (aggressive (prev-criminal-cperson) cb-ap)
   ))

; Note:
; It's desirable (required?) to give different names to person objects
; here and in crime-propns.  Even though they're persons, they're not
; really the same persons playing a role in a crime scenario and in 
; a beast scenario.  They can/should get identified by the analogizing
; process, but the process can do that.
; So rather than calling what's attacked an at-risk-cperson (like people
; who might become criminals) or a cperson (like those who are
; harmed by crime), we use another term for potential beast victims.

(defvar beast-propns
  '(
    (human (bperson) b-pp)   ; supposed to match: (not-criminal (cperson) cb-np)
    (attack (beast bperson) b-abp)
    (harms (bperson) b-hp)
    (cause (b-abp b-hp) b-abp->hp) ; being attacked is harmful
    ; added 11/8/12:
    (helps (beast) b-hb)
    (cause (b-abp b-hb) b-abp->hb) ; beasts benefit from attacking--e.g get food


    (capture (beast) b-cpb)
    (prevent (b-cpb b-abp) b-cpb->-abp)
    ; adding any of these doesn't make much difference:
    ;(PREVENT (B-CPB B-HP) B-CPB->-HP) ; snap the transitive pointers
    ;(kill (beast) b-kb)
    ;(prevent (b-kb b-abp) b-kp->-abp)

    (aggressive (beast) b-ab)
   ))


(defvar crime-propns `(,@viral-crime-propns ,@beastly-crime-propns))

(defvar virus-propn-syms (mapcar #'third virus-propns))
(defvar beast-propn-syms (mapcar #'third beast-propns))
(defvar viral-crime-propn-syms (mapcar #'third viral-crime-propns))
(defvar beastly-crime-propn-syms (mapcar #'third beastly-crime-propns))
(defvar crime-propn-syms (mapcar #'third crime-propns))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Note that I'm reversing source vs target wrt the sanday simulations:

(defun make-no-bias-crime-talker (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start (,@crime-propns)))
                   (make-struc 'source 'problem '(start ()))
                   ,@semantic-relations)
                 '()
                 '(target)))

(defun make-virus-bias-crime-talker (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start (,@crime-propns)))
                   (make-struc 'source 'problem '(start (,@virus-propns)))
                   ,@semantic-relations)
                 '()
                 '(target)))

(defun make-beast-bias-crime-talker (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start (,@crime-propns)))
                   (make-struc 'source 'problem '(start (,@beast-propns)))
                   ,@semantic-relations)
                 '()
                 '(target)))

(defun make-both-bias-crime-talker (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start (,@crime-propns)))
                   (make-struc 'source 'problem '(start (,@virus-propns ,@beast-propns)))
                   ,@semantic-relations)
                 '()
                 '(target)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-virus-bias-all-talker (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start (,@crime-propns)))
                   (make-struc 'source 'problem '(start (,@virus-propns)))
                   ,@semantic-relations)
                 '()
                 '()))

(defun make-beast-bias-all-talker (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start (,@crime-propns)))
                   (make-struc 'source 'problem '(start (,@beast-propns)))
                   ,@semantic-relations)
                 '()
                 '()))

(defun make-both-bias-all-talker (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start (,@crime-propns)))
                   (make-struc 'source 'problem '(start (,@virus-propns ,@beast-propns)))
                   ,@semantic-relations)
                 '()
                 '()))

(defun make-no-bias-all-talker (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start (,@crime-propns)))
                   (make-struc 'source 'problem '(start ()))
                   ,@semantic-relations)
                 '()
                 '()))

(defun make-naive-all-talker (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start ()))
                   (make-struc 'source 'problem '(start ()))
                   ,@semantic-relations)
                 '()
                 '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; leave these at defaults:
; *propn-excit-weight* *propn-inhib-weight* *trust* *perceived-excit*
;(setf *time-runs* nil)
;(setf *do-converse* t)  ; set this in particular model files
(setf *do-update-propn-nets* t)
(setf *do-report-to-netlogo* t)
(setf *do-report-propns-to-csv* t)
(setf *do-report-analogy-nets-to-guess* t)
(setf *sleep-delay* nil)           ; If non-nil, pause this many seconds between generations
(setf *silent-run?* t)             ; If nil, use Thagard-style verbose reporting to console

;; first clear everything out
(mapcar #'clear-plists (get 'folks 'members))
(clear-person-nets 'folks)
;(kill-everyone)
(setf *the-population* 'folks)
