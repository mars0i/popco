

; MAYBE I NEED TO ADD TIME INDEXES

(defvar virus-propns
  '(
    (infects-from-to (infected uninfected) v-iiu)
    (are-infected (infected) v-ai)
    (cause (v-iiu v-ai) v-iiu->ai) ; transmission from infected to uninfected causes infection
    (harm (infected) v-hi)
    (cause (v-ai v-hi) v-ai->hi) ; being infected causes harm

    (quarantine (infected) v-qi)
    (prevent (v-qi v-iiu) v-qi->-iiu) ; quarantining the infected prevents further infection
    (innoculate (uninfected) v-iu)
    (prevent (v-iu v-iiu) v-qi->-iiu) ; innoculation of uninfected prevents further infection
    (treat (infected) v-ti)
    (prevent (v-ti v-iiu) v-qi->-iiu) ; treatment of infected prevents further infection
   ))

(defvar virus-semantic-relations
  '(
    (similar 'cause 'prevent (* -1 *ident-weight*)) ; avoid mapping cause to prevent

    ; How to capture that if prevention occurs, it prevents/reduces infection?  or is that necessary?
    ; The problem is that we don't actually have logical inference.  
    ; [Add the extra credence/truth-value arg?  That allows additional semblance of negation.]
    ; What I want to say is that if antecedent, and if (prevents antecedent consequent), then
    ; not consequent.  Here consequent is v-iiu.

    ;(semantic-iff 'propn1 'propn2 .5)
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

