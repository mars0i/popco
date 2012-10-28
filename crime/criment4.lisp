
; Notational conventions:
; v-: virus
; cv-: virus-ey crime
; cb-: beast-ey crime
; b-: beast
; X->Y: X causes Y to occur, where X and Y are propositions
; X->-Y: X prevents Y from occuring, where X and Y are propositions

(setf *propn-category-prefixes* '("V" "B" "CV" "CB"))
(setf *propn-category-descriptions* '("virus propns" "beast propns" "virus-like crime propns" "beast-like crime propns"))


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

(defvar virus-propns
  '(
    (is-infected (prev-infected-elt) v-ip) ; the existence of a previously infected element is assumed
    (not-infected (at-risk-elt) v-na)  ; at risk element is not infected
    (is-infected (at-risk-elt) v-ia)   ; at-risk person/thing is infected

    (harmed (at-risk-elt) v-ha)        ; note we have no quantificatiion or true pattern matching
    (cause (v-ia v-ha) v-ciha)

    (harmed (prev-infected-elt) v-hp)  ; drop? introduces noise?
    (cause (v-ip v-hp) v-cihp)         ; drop? introduces noise?

    (spread-from-to (prev-infected-elt at-risk-elt) v-spa) ; infection spreads from the previously infected to the at-risk
    (cause (v-spa v-ia) v-spa->ia) ; transmission from infected to uninfected causes infection

    ; The following triplets are a bit awkward and convoluted because we don't have time indexing:

    (innoculate (at-risk-elt) v-ica)   ; innoculating the at-risk prevents spread to new individuals (or something with cells?)
    (prevent (v-ica v-spa) v-ia->-spa) ; innoculation of uninfected prevents further infection
    (cause (v-ia->-spa v-na) v-iaspa->na) ; preventing spread of infection causes [preserves] lack of infection in the at-risk

    ; Next two triplets are structurally identical. Maybe drop one.

    (quarantine (prev-infected-elt) v-qp) ; previously infected is quarantined (or cells sequestered, I suppose)
    (prevent (v-qp v-spa) v-qp->-spa) ; quarantining the infected prevents spread to new individuals 
    (cause (v-qp->-spa  v-na) v-qpspa->na)

    (treat (prev-infected-elt) v-tp)  ; treatment of the infected to remove disease prevents later spread
    (prevent (v-tp v-spa) v-tp->-spa) ; treatment of infected prevents further infection
    (cause (v-tp->-spa v-na) v-tpspa->na)
    ; [We elide the step in which the infected becomes uninfected, which would require time-indexing.]
   ))

(defvar virus-semantic-relations
  '(
    (similar 'cause 'prevent (* -1 *ident-weight*)) ; avoid mapping cause to prevent
    (semantic-iff 'v-ia 'v-na -1.0) ; infecting and preventing infection are inconsistent [but is -1 too strong?]
                                    ; i.e. given the modeling simplification that there is only one at-risk indiv.
   ))

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

(defvar crime-propns
  '(
    (is-criminal (prev-criminal-pers) cv-cp) ; person who's already committing crimes
    (not-criminal (at-risk-pers) cv-na)      ; person at risk of turning to crime
    (is-criminal (at-risk-pers) cv-ca)
    (not-criminal (no-risk-pers) cv-nn)   ; person not at risk of turning to crime

    (harmed (at-risk-pers) cv-ha)
    (cause (cv-ca cv-ha) cv-ccha)         ; becoming a criminal has bad consequences for the individual

    (harmed (prev-criminal-pers) cv-hp)
    (cause (cv-cp cv-hp) cv-cchp)         ; being a criminal has bad consequences for the criminal

    (spread-from-to (prev-criminal-pers at-risk-pers) cv-spa)
    (cause (cv-spa cv-ca) cv-sca->ca) ; 

    ; The following triplets are a bit awkward and convoluted because we don't have time indexing:

    (support (at-risk-elt) cv-sa) ; support = financial support, supportive parents, mentors, etc., education, etc.
    (prevent (cv-sa cv-spa) cv-sa->-spa)
    (cause (cv-sa->-spa cv-na) cv-saspa->na)

    ; Next two triplets are structurally identical. Maybe drop one.

    (imprison (prev-criminal-pers) cv-ip)
    (prevent (cv-ip cv-spa) cv-ip->-spa)
    (cause (cv-ip->-spa  cv-na) cv-ipspa->na) ; imprisoning prevents crime

    (reform (prev-criminal-pers) cv-rp) ; reform includes social support, education, etc. to criminals
    (prevent (cv-rp cv-spa) cv-rp->-spa)
    (cause (cv-rp->-spa cv-na) cv-rpspa->na)

    ; In addition to criminals being harmed by being criminals,
    ; non-criminals are harmed by criminals [not analogous to virus]:

    (harmed (no-risk-pers) cv-hn)
    (cause (cv-cp cv-hn) cv-cp->hn) ; existing criminals harm those not at risk
    (cause (cv-ca cv-hn) cv-ca->hn) ; at-risk persons do too, if they turn to crime

    ; TODO:
    ; THEN DO BEASTLY-CRIME

   ))



(defvar beast-propns
  '(

    ; These are stolen from the Sanday simulations:

    ; danger
    (hunts-endangers (people beast) b-Person-Endangers-Beast)
    (harms (beast people) b-Beast-Harms-Person)
    (causes (b-Person-Endangers-Beast b-Beast-Harms-Person) b-Hunting-Is-Dangerous)

    ;; social location:
    (distant-agent (beast people) b-Beast-Distant)
   ))


; Note that I'm reversing source vs target wrt the sanday simulations:

(defun make-test-person (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start (,@crime-propns)))
                   (make-struc 'source 'problem '(start (,@virus-propns)))
                   ,@virus-semantic-relations)
                 '()
                 '(target)))


; leave these at defaults:
; *propn-excit-weight* *propn-inhib-weight* *trust* *perceived-excit*
;(setf *time-runs* nil)
(setf *do-converse* nil)
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

(make-test-person 'andy crime-propns)

(print (get 'folks 'members))

(setf *max-pop-ticks* 200)
(init-pop)

