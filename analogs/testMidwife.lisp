; testMidwife.lisp

;; first clear everything out
(clear-person-nets 'philosophers)
(clear-plists
  'carnap ; persons "ADD HERE"
  'philosophers)                   ; populations/groups

(format t "Making philosophers...~%")
(make-person 'carnap 'philosophers
             '()
             '((make-struc 'source 'problem
                           '(start
                              ((midwife (obj_midwife) m1)
                               (mother (obj_mother) m2 m2)
                               (father (obj_father) m3)
                               (child (obj_child) m4)
                               (matches (obj_midwife obj_mother obj_father) m5)
                               (conceives (obj_mother obj_child) m6)
                               (cause (m5 m6) m7)
                               (in_labor_wlth (obj_mother obj_child) m9)
                               (helps (obj_midwife obj_mother) m10)
                               (give_birth_to (obj_mother obj_child) m11)
                               (cause (m10 m11) m12))))
               (make-struc 'target 'problem
                           '(start 
                              ((father (Socrates) S20) 
                               (poison (obj_hemlock) S21) 
                               (drink (Socrates obj_hemlock) S22) 
                               (midwife (obj_soc_midwife) S23) 
                               (mother (obj_soc_wife) S24) 
                               (matches (obj_soc_midwife obj_soc_wife Socrates) S25) 
                               (child (obj_soc_child) S26) 
                               (conceives (obj_soc_wife obj_soc_child) S27) 
                               (cause (S25 S27) S28) 
                               (in_labor_with (obj_soc_wife obj_soc_child) S29) 
                               (helps (obj_soc_midwife obj_soc_wife) S30) 
                               (give_birth_to (obj_soc_wife obj_soc_child) S31) 
                               (cause (S30 S31) S32))))
               (constraint-map 'midwife 'socrates)))

(setf phils 'philosophers)

(format t "Philosophers made: ")
(my-print (get 'philosophers 'members))
