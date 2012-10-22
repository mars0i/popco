
(defvar beast-propns
  '(
    ; danger
    (hunts-endangers (people beast) h-People-Endangers-Beast)
    (harms (beast people) h-Beast-Harms-People)
    (causes (h-People-Endangers-Beast h-Beast-Harms-People) h-Hunting-Is-Dangerous)

    ;; social location:
    (distant-agent (beast people) h-Beast-Distant)
   ))

