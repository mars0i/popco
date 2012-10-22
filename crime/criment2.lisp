

; MAYBE I NEED TO ADD TIME INDEXES

(defvar virus-propns
  '(
    (infects-from-to (virus infected uninfected) v-iviu)
    (are-infected (infected) v-ai)
    (cause (v-iviu v-ai) v-iviu->ai) ; transmission from infected to uninfected causes infection
    (harm (infected) v-hi)
    (cause (v-ai v-hi) v-ai->hi) ; being infected causes harm
    (quarantine (infected) v-qi)
    (prevent (v-qi v-iviu) v-qi->-iviu) ; quarantining the infected prevents further infection
    (innoculate (uninfected) v-iu)
    (prevent (v-iu v-iviu) v-qi->-iviu) ; innoculation of uninfected prevents further infection
    (treat (infected) v-ti)
    (prevent (v-ti v-iviu) v-qi->-iviu) ; treatment of infected prevents further infection
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

