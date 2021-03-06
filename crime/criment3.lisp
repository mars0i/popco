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

(defvar virus-propns
  '(
    (is-infected (prev-infected-elt) v-ip) ; the existence of a previously infected element is assumed
    (is-infected (at-risk-elt) v-ia)   ; at-risk person/thing is infected
    ;(not-infected (at-risk-elt) v-na)  ; at risk element is not infected

    (harmed (at-risk-elt) v-ha)        ; note we have no quantificatiion or true pattern matching
    (cause (v-ia v-ha) v-ciha)

    (harmed (prev-infected-elt) v-hp)  ; drop? introduces noise?
    (cause (v-ip v-hp) v-cihp)         ; drop? introduces noise?

    (spread-from-to (prev-infected-elt at-risk-elt) v-spa) ; infection spreads from the previously infected to the at-risk
    (cause (v-spa v-ia) v-spa->ia) ; transmission from infected to uninfected causes infection

; I HAVE NOTHING SO FAR THAT SAYS THAT PREVENTION OPPOSES INFECTION:

    (innoculate (at-risk-elt) v-iu)   ; innoculating the at-risk prevents spread to new individuals (or something with cells?)
    (prevent (v-iu v-spa) v-iu->-spa) ; innoculation of uninfected prevents further infection

    ; Next two pairs structurally identical. Maybe drop one.
    (quarantine (prev-infected-elt) v-qp) ; previously infected is quarantined (or cells sequestered, I suppose)
    (prevent (v-qp v-spa) v-qp->-spa) ; quarantining the infected prevents spread to new individuals 
    (treat (prev-infected-elt) v-tp)  ; treatment of the infected to remove disease prevents later spread
    (prevent (v-tp v-spa) v-qp->-spa) ; treatment of infected prevents further infection
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
    (is-criminal (prev-criminal-pers) c-cp)
    (is-criminal (at-risk-pers) c-ca)
    (not-criminal (at-risk-pers) c-na)

    (harmed (at-risk-pers) c-ha)
    (cause (c-ca c-ha) c-ccha)         ; becoming a criminal has bad consequences for the individual

    (harmed (prev-criminal-pers) c-hp)
    (cause (c-cp c-hp) c-cchp)         ; being a criminal has bad consequences for the criminal

    (spread-from-to (prev-criminal-pers at-risk-pers) c-spa)
    (cause (c-spa c-ia) c-sca->ia) ; 

    (support (at-risk-pers) v-iu)
    (prevent (v-iu v-spa) v-qc->-spa)

    ; Next two pairs structurally identical. Maybe drop one.
    (imprison (prev-criminal-pers) c-ip)
    (prevent (v-qc v-spa) v-qc->-spa)
    (reform (prev-criminal-pers) v-tp)
    (prevent (v-tc v-spa) v-qc->-spa)


    ; TODO:
    ; ADD ADDL PROPNS ABOUT HARMING INNOCENT PEOPLE
    ; THEN DO BEAST AND BEASTLY-CRIME

   ))



(defvar beast-propns
  '(
    ; danger
    (hunts-endangers (people beast) h-Person-Endangers-Beast)
    (harms (beast people) h-Beast-Harms-Person)
    (causes (h-Person-Endangers-Beast h-Beast-Harms-Person) h-Hunting-Is-Dangerous)

    ;; social location:
    (distant-agent (beast people) h-Beast-Distant)
   ))

