;; HTsOriginalSocratesAllMapUnits.lisp
;;
;; Defines variables for three lists of lists of map units used 
;; in Socrates analogies, grouped by target, with groups
;; ordered by the order H&T1989's table 20 on p. 346. The three
;; lists defined here together contain all of the map units
;; created in the two non-isomorphic analogies (one with, one without
;; pragmatic specifications).  Only the object and predicate mappings
;; are listed in table 20, which only lists highest-valued activations,
;; so the proposition maps are simply listed in numeric order, which
;; is the order they appear in H&T's source code.


; object mappings
(defvar soc-obj-maps '((SOCRATES=OBJ-CHILD
                         SOCRATES=OBJ-FATHER
                         SOCRATES=OBJ-MIDWIFE
                         SOCRATES=OBJ-MOTHER)

                       (OBJ-STUDENT=OBJ-CHILD
                         OBJ-STUDENT=OBJ-FATHER
                         OBJ-STUDENT=OBJ-MIDWIFE
                         OBJ-STUDENT=OBJ-MOTHER)

                       (OBJ-PARTNER=OBJ-CHILD
                         OBJ-PARTNER=OBJ-FATHER
                         OBJ-PARTNER=OBJ-MIDWIFE
                         OBJ-PARTNER=OBJ-MOTHER)

                       (OBJ-IDEA=OBJ-CHILD
                         OBJ-IDEA=OBJ-FATHER
                         OBJ-IDEA=OBJ-MIDWIFE
                         OBJ-IDEA=OBJ-MOTHER)

                       (OBJ-SOC-MIDWIFE=OBJ-CHILD
                         OBJ-SOC-MIDWIFE=OBJ-FATHER
                         OBJ-SOC-MIDWIFE=OBJ-MIDWIFE
                         OBJ-SOC-MIDWIFE=OBJ-MOTHER)

                       (OBJ-SOC-WIFE=OBJ-CHILD
                         OBJ-SOC-WIFE=OBJ-FATHER
                         OBJ-SOC-WIFE=OBJ-MIDWIFE
                         OBJ-SOC-WIFE=OBJ-MOTHER)

                       (OBJ-SOC-CHILD=OBJ-CHILD
                         OBJ-SOC-CHILD=OBJ-FATHER
                         OBJ-SOC-CHILD=OBJ-MIDWIFE
                         OBJ-SOC-CHILD=OBJ-MOTHER)

                       (OBJ-HEMLOCK=OBJ-CHILD
                         OBJ-HEMLOCK=OBJ-FATHER
                         OBJ-HEMLOCK=OBJ-MIDWIFE
                         OBJ-HEMLOCK=OBJ-MOTHER)))

; predicate mappings
(defvar soc-pred-maps '((PHILOSOPHER=CHILD
                          PHILOSOPHER=FATHER
                          PHILOSOPHER=MIDWIFE
                          PHILOSOPHER=MOTHER)

                        (STUDENT=CHILD
                          STUDENT=FATHER
                          STUDENT=MIDWIFE
                          STUDENT=MOTHER)

                        (INTELLECTUAL-PARTNER=CHILD
                          INTELLECTUAL-PARTNER=FATHER
                          INTELLECTUAL-PARTNER=MIDWIFE
                          INTELLECTUAL-PARTNER=MOTHER)

                        (IDEA=CHILD
                          IDEA=FATHER
                          IDEA=MIDWIFE
                          IDEA=MOTHER)

                        (INTRODUCE=MATCHES)

                        (FORMULATES=CONCEIVES
                          FORMULATES=GIVE-BIRTH-TO
                          FORMULATES=HELPS
                          FORMULATES=IN-LABOR-WITH)

                        (THINKS-ABOUT=CONCEIVES
                          THINKS-ABOUT=GIVE-BIRTH-TO
                          THINKS-ABOUT=HELPS
                          THINKS-ABOUT=IN-LABOR-WITH)

                        (TESTS-TRUTH=CONCEIVES
                          TESTS-TRUTH=GIVE-BIRTH-TO
                          TESTS-TRUTH=HELPS
                          TESTS-TRUTH=IN-LABOR-WITH)

                        (KNOWS-TRUTH-OR-FALSITY=CONCEIVES
                          KNOWS-TRUTH-OR-FALSITY=GIVE-BIRTH-TO
                          KNOWS-TRUTH-OR-FALSITY=HELPS
                          KNOWS-TRUTH-OR-FALSITY=IN-LABOR-WITH)

                        (HELPS=CONCEIVES
                          HELPS=GIVE-BIRTH-TO
                          HELPS=HELPS
                          HELPS=IN-LABOR-WITH)

                        (CAUSE=CAUSE)

                        (POISON=CHILD
                          POISON=FATHER
                          POISON=MIDWIFE
                          POISON=MOTHER)

                        (DRINK=CONCEIVES
                          DRINK=GIVE-BIRTH-TO
                          DRINK=HELPS
                          DRINK=IN-LABOR-WITH)

                        (FATHER=CHILD
                          FATHER=FATHER
                          FATHER=MIDWIFE
                          FATHER=MOTHER)

                        (MIDWIFE=CHILD
                          MIDWIFE=FATHER
                          MIDWIFE=MIDWIFE
                          MIDWIFE=MOTHER)

                        (MOTHER=CHILD
                          MOTHER=FATHER
                          MOTHER=MIDWIFE
                          MOTHER=MOTHER)

                        (CHILD=CHILD
                          CHILD=FATHER
                          CHILD=MIDWIFE
                          CHILD=MOTHER)

                        (MATCHES=MATCHES)

                        (CONCEIVES=CONCEIVES
                          CONCEIVES=GIVE-BIRTH-TO
                          CONCEIVES=HELPS
                          CONCEIVES=IN-LABOR-WITH)

                        (IN-LABOR-WITH=CONCEIVES
                          IN-LABOR-WITH=GIVE-BIRTH-TO
                          IN-LABOR-WITH=HELPS
                          IN-LABOR-WITH=IN-LABOR-WITH)

                        (GIVE-BIRTH-TO=CONCEIVES
                          GIVE-BIRTH-TO=GIVE-BIRTH-TO
                          GIVE-BIRTH-TO=HELPS
                          GIVE-BIRTH-TO=IN-LABOR-WITH)))

; proposition mappings
(defvar soc-propn-maps '((S1=M1
                           S1=M2
                           S1=M3
                           S1=M4)

                         (S2=M1
                           S2=M2
                           S2=M3
                           S2=M4)

                         (S3=M1
                           S3=M2
                           S3=M3
                           S3=M4)

                         (S4=M1
                           S4=M2
                           S4=M3
                           S4=M4)

                         (S5=M10
                           S5=M5)

                         (S6=M10
                           S6=M11
                           S6=M6
                           S6=M8)

                         (S7=M12
                           S7=M7)

                         (S8=M10
                           S8=M11
                           S8=M6
                           S8=M8)

                         (S9=M10
                           S9=M11
                           S9=M6
                           S9=M8)

                         (S10=M10
                           S10=M11
                           S10=M5
                           S10=M6
                           S10=M8)

                         (S11=M10
                           S11=M11
                           S11=M6
                           S11=M8)

                         (S12=M12
                           S12=M7)

                         (S20=M1
                           S20=M2
                           S20=M3
                           S20=M4)

                         (S21=M1
                           S21=M2
                           S21=M3
                           S21=M4)

                         (S22=M10
                           S22=M11
                           S22=M6
                           S22=M8)

                         (S23=M1
                           S23=M2
                           S23=M3
                           S23=M4)

                         (S24=M1
                           S24=M2
                           S24=M3
                           S24=M4)

                         (S25=M10
                           S25=M5)

                         (S26=M1
                           S26=M2
                           S26=M3
                           S26=M4)

                         (S27=M10
                           S27=M11
                           S27=M6
                           S27=M8)

                         (S28=M12
                           S28=M7)

                         (S29=M10
                           S29=M11
                           S29=M6
                           S29=M8)

                         (S30=M10
                           S30=M11
                           S30=M5
                           S30=M6
                           S30=M8)

                         (S31=M10
                           S31=M11
                           S31=M6
                           S31=M8)

                         (S32=M12
                           S32=M7)))

; all mappings
(defvar soc-all-maps (append soc-obj-maps soc-pred-maps soc-propn-maps))
