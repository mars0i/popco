(defvar virus-propns
  '(
    (infects (virus person) v-Infects-Virus-Person)
    (are-infected (person) v-Are-Infected-Person)
    (harm (person) v-Harm-Person)
    (cause (v-Infects-Virus-Person v-Is-Infected-Person) v-Virus-Causes-Infection)
    (cause (v-Is-Infected-Person v-Person-Harm) v-Infection-Causes-Harm)
   ))

(defvar beast-propns
  '(
    ; danger
    (hunts-endangers (person beast) h-Person-Endangers-Beast)
    (harms (beast person) h-Beast-Harms-Person)
    (causes (h-Person-Endangers-Beast h-Beast-Harms-Person) h-Hunting-Is-Dangerous)

    ;; social location:
    (distant-agent (beast person) h-Beast-Distant)
   ))

