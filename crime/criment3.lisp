
(defvar virus-propns
  '(
    (is-infected (curr-infected-elt) v-ic)

    ; Both of these possibilities will play a role.
    ; They are made contradictory via a semantic statement below.
    (is-infected (at-risk-elt) v-ia)
    (not-infected (at-risk-elt) v-na)

    ; We have to list connections between infection and harm explicitly
    ; since ACME doesn't have universal quantification or proper pattern matching.
    (harmed (at-risk-elt) v-ha)
    (cause (v-ia v-ha) v-ciha)
    ; These two should probably be dropped--not needed, and introduce noise:
    (harmed (curr-infected-elt) v-hc)
    (cause (v-ic v-hc) v-cihc)

    (spread-from-to (curr-infected-elt at-risk-elt) v-sca)
    (cause (v-sca v-ia) v-sca->ia) ; transmission from infected to uninfected causes infection

    (quarantine (curr-infected-elt) v-qc)
    (prevent (v-qc v-sca) v-qc->-sca) ; quarantining the infected prevents further infection
    (innoculate (at-risk-elt) v-iu)
    (prevent (v-iu v-sca) v-qc->-sca) ; innoculation of uninfected prevents further infection
    (treat (curr-infected-elt) v-tc)
    (prevent (v-tc v-sca) v-qc->-sca) ; treatment of infected prevents further infection
   ))

(defvar virus-semantic-relations
  '(
    (similar 'cause 'prevent (* -1 *ident-weight*)) ; avoid mapping cause to prevent
    (semantic-iff 'v-ia 'v-na -1.0) ; is this too strong?

    ; How to capture that if prevention occurs, it prevents/reduces infection?  or is that necessary?
    ; The problem is that we don't actually have logical inference.  
    ; [Add the extra credence/truth-value arg?  That allows additional semblance of negation.]
    ; What I want to say is that if antecedent, and if (prevents antecedent consequent), then
    ; not consequent.  Here consequent is v-sca.

   ))

(defvar virus-goal-propns
  '(
     ; something capturing preventing harm to as many people as possible
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

