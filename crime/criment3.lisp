
(defvar virus-propns
  '(
    (is-infected (prev-infected-elt) v-ip) ; the existence of a previously infected element is assumed

    ; Both of these possibilities will play a role.
    ; They are made contradictory via a semantic statement below.
    (is-infected (at-risk-elt) v-ia)   ; at-risk person/thing is infected
    (not-infected (at-risk-elt) v-na)  ; at risk element is not infected

    ; We have to list connections between infection and harm explicitly for each person/thing
    ;  since ACME doesn't have universal quantification or proper pattern matching:
    ; These first two should maybe be dropped--not needed, and may distract ACME:
    (harmed (prev-infected-elt) v-hp)
    (cause (v-ip v-hp) v-cihp)
    ; These next two definitely should be retained:
    (harmed (at-risk-elt) v-ha)
    (cause (v-ia v-ha) v-ciha)

    (spread-from-to (prev-infected-elt at-risk-elt) v-sca) ; infection spreads from the previously infected to the at-risk
    (cause (v-sca v-ia) v-sca->ia) ; transmission from infected to uninfected causes infection

    (innoculate (at-risk-elt) v-iu)   ; innoculating the at-risk prevents spread to new individuals (or something with cells?)
    (prevent (v-iu v-sca) v-qc->-sca) ; innoculation of uninfected prevents further infection

    ; We should probably use only one of the next two pairs, since they're structurally identical;
    ;  leaving in both, with no differences, leads them to compete and confuse ACME.

    (quarantine (prev-infected-elt) v-qp) ; previously infected is quarantined (or cells sequestered, I suppose)
    (prevent (v-qc v-sca) v-qc->-sca) ; quarantining the infected prevents spread to new individuals 

    ; Treatment of the infected prevents spread to new individuals.
    ;  We elide the step in which the infected becomes uninfected, which would require time-indexing.
    (treat (prev-infected-elt) v-tp)  ; treatment of the infected to remove disease prevents later spread
    (prevent (v-tc v-sca) v-qc->-sca) ; treatment of infected prevents further infection
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
    ; these next few parallel the first few in virus-propns; see comments there.

    (is-criminal (prev-criminal) c-cp)
    (is-criminal (at-risk-pers) c-ca)
    (not-criminal (at-risk-pers) c-na)

    (harmed (prev-criminal) c-hp)
    (cause (c-cp c-hp) c-cchp)
    (harmed (at-risk-pers) c-ha)
    (cause (c-ca c-ha) c-ccha)

    ; TODO:
    ; ADD SPREAD/PREVENTION PROPNS
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

