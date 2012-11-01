

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

(setf *propn-category-prefixes* '("V" "B" "CV" "CB"))
(setf *propn-category-descriptions* '("virus propns" "beast propns" "virus-like crime propns" "beast-like crime propns"))

; Question: How *is* crime like a virus?  What are the analogs of
; infection, people, cells, etc.  In the case of viruses, the virus is the
; essentially harming element, and it is transmitted from the harmed to
; the not yet harmed.  In the case of crime, being a criminal is
; transmitted, but crime harms the non-criminals more than criminals
; [according to common ways of thinking, at least, and contrary to Socrates' view].

; On the other hand, it's not clear that the intuition that criminality spreads
; from one individual to another is that clear.  I guess it is reflected in some
; of the preventative measures that people entertain.  But I feel that a lot
; of virusey thinking about crime is vague.

; The main difference between disease and criminality is that although
; criminality spreads, harming the new criminals, its main harmful effects
; are on non-criminals.  Disease can have a similar effect, in that some
; of those infected might not pass it on; they are like innocent vicitms
; of crime.  But that's less common and less central than in the crime
; case, and the negative effects fall heavier on crime victims than
; criminals, whereas typically non-transmitted infections are not worse
; than the transmitted ones (although variation in virulence can create
; this effect sometimes).  Note that vampirism is like a virus with the
; side-effects on innocents of crime.  Drug addiction and sometimes
; alcoholism are similar, sometimes.

; Also note that imprisoning criminals does not get rid of all crime,
; since at the very least the crime for which the criminal was
; convicted occured.  The same is true of reform of criminals.
; (On the other hand, imprisonment serves not only to take the criminal
; off the street, but also to deter other crimes.)

; Whereas support to prevent individuals from committing crimes could
; in principle get rid of all crime, just as innoculation and other
; preventative measures could in principle get rid of all infections.

; Note that there's also a well-known effect of imprisoning causing
; more criminality after a prisoner is released.

; NOTE THESE ARE SIMPLY BASED ON MY INTUITIONS (philosophy/linguistics style)
; AND REALLY OUGHT TO COME FROM DATA.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar general-semantic-relations
  '(
    (similar 'cause 'prevent (* -1 *ident-weight*)) ; avoid mapping cause to prevent
   ))

; Note we call the things that get infected "elts", i.e. elements,
; since they could either be people or cells or something like that.
; Also see note about naming person objects the same elsewhere in this file.

(defvar virus-propns
  '(
    (is-infected (prev-infected-elt) v-ip) ; the existence of a previously infected element is assumed
    (not-infected (at-risk-elt) v-na)  ; at risk element is not infected
    (is-infected (at-risk-elt) v-ia)   ; at-risk person/thing is infected

    (harmed (at-risk-elt) v-ha)        ; note we have no quantificatiion or true pattern matching
    (cause (v-ia v-ha) v-ciha)

    (harmed (prev-infected-elt) v-hp)  ; drop? introduces noise?
    (cause (v-ip v-hp) v-cihp)         ; drop? introduces noise?

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

(defvar virus-semantic-relations
  '(
    (similar 'is-infected 'not-infected (* -1 *ident-weight*))
    ; do we need this next one given preceding?:
    ;(semantic-iff 'v-ia 'v-na -1.0) ; at-risk-cperson being infected and being uninfected are inconsistent [-1 too strong?]
   ))

(defvar viral-crime-propns
  '(
    (is-criminal (prev-criminal-cperson) cv-cp) ; person who's already committing crimes
    (not-criminal (at-risk-cperson) cv-na)      ; person at risk of turning to crime
    (is-criminal (at-risk-cperson) cv-ca)

    (harmed (at-risk-cperson) cv-ha)
    (cause (cv-ca cv-ha) cv-ca->hp)         ; becoming a criminal has bad consequences for the individual

    (harmed (prev-criminal-cperson) cv-hp)
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
    ; In addition to criminals being harmed by being criminals,
    ; non-criminals are harmed by criminals [not analogous to virus]:
    ; I'm calling the next group "cb-" because in theory they should map with
    ; beast propns but not virus propns: virus propns only harm by making the
    ; harmed into carriers [like leading a non-criminal into crime], whereas
    ; beasts harm anyone; they don't turn victims into beasts.
    ; QUESTION: Should I codify this last point in propositions?

    (not-criminal (cperson) cb-np)   ; person not at risk of turning to crime
    (victimize (prev-criminal-cperson cperson) cb-vpp)
    (harmed (cperson) cb-hcp) ; hp already in use as a name
    (cause (cb-vpp cv-hcp) cb-vpp->hcp) ; existing criminals harm those not at risk
    ; old versions:
    ;(cause (cv-cp cv-hcp) cb-cp->hcp) ; existing criminals harm those not at risk
    ;(cause (cv-ca cv-hcp) cb-ca->hcp) ; at-risk persons do too, if they turn to crime

    (capture (prev-criminal) cb-cpc) ; cp is already used as name for crime propn
    (prevent (cb-cpc cb-vpp) cb-cpc->-vpp)
    ;(kill (prev-criminal) cb-kp)
    ;(prevent (cb-kp cb-vpp) cb-kp->-vpp)

    (aggressive (prev-criminal) cb-ap)
   ))

(defvar crime-propns `(,@viral-crime-propns ,@beastly-crime-propns))

(defvar crime-semantic-relations
  '(
    (similar 'is-criminal 'not-criminal (* -1 *ident-weight*))
    ; do we need this next one given preceding?:
    ;(semantic-iff 'cv-ca 'cv-na -1.0) ; at-risk-cperson being infected and being uninfected are inconsistent [-1 too strong?]
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
    (beastly (beast) b-bb)  ; supposed to match: (is-criminal (prev-criminal-cperson) cv-cp)

    (human (bperson) b-pp)   ; supposed to match: (not-criminal (cperson) c-np)
    (attacks (beast bperson) b-abp)
    (harmed (bperson) b-hp)
    (cause (b-abp b-hp) b-abp->hp) ; being attacked is harmful
    ; simple version:
    ;(cause (b-bb b-hp) b-bb->hp) ; beasts harm persons


    (capture (beast) b-cpb)
    (prevent (b-cpb b-abp) b-cpb->-abp)
    ;(kill (beast) b-kb)
    ;(prevent (b-kb b-abp) b-kp->-abp)

    (aggressive (beast) b-ab)

    ; The following are borrowed from the Sanday simulations:
    ;(hunts-endangers (people beast) b-Person-Endangers-Beast)
    ;(harms (beast people) b-Beast-Harms-Person)
    ;(causes (b-Person-Endangers-Beast b-Beast-Harms-Person) b-Hunting-Is-Dangerous)
    ;(distant-agent (beast people) b-Beast-Distant)
   ))

(defvar beast-semantic-relations
  '(
    (similar 'beastly 'human (* -.5 *ident-weight*))
    ; do we need this next one given preceding?:
    ;(semantic-iff 'cv-ca 'cv-na -1.0) ; at-risk-cperson being infected and being uninfected are inconsistent [-1 too strong?]
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Note that I'm reversing source vs target wrt the sanday simulations:

(defun make-no-bias-crime-talker (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start (,@crime-propns)))
                   (make-struc 'source 'problem '(start ()))
                   ,@general-semantic-relations)
                 '()
                 '(target)))

(defun make-virus-bias-crime-talker (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start (,@crime-propns)))
                   (make-struc 'source 'problem '(start (,@virus-propns)))
                   ,@general-semantic-relations
                   ,@crime-semantic-relations
                   ,@virus-semantic-relations)
                 '()
                 '(target)))

(defun make-beast-bias-crime-talker (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start (,@crime-propns)))
                   (make-struc 'source 'problem '(start (,@beast-propns)))
                   ,@general-semantic-relations
                   ,@crime-semantic-relations
                   ,@virus-semantic-relations
                   ,@beast-semantic-relations)
                 '()
                 '(target)))

(defun make-both-bias-crime-talker (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start (,@crime-propns)))
                   (make-struc 'source 'problem '(start (,@virus-propns ,@beast-propns)))
                   ,@general-semantic-relations
                   ,@crime-semantic-relations
                   ,@beast-semantic-relations)
                 '()
                 '(target)))

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
(setf *the-population* 'folks)
