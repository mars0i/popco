;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODEL PERSONS

; The template:
;(make-person 'name-of-person *the-population* propns-to-be-perceived-initially
;             `((make-struc 'target 'problem '(start (,@propns-to-subst-in-here)))
;               (make-struc 'source 'problem '(start (,@propns-to-subst-in-here)))
;               ,@semantic-specs-to-subst-in-here)
;             `(@,pragmatic-relations)
;             '()) ; put 'source or 'target in list to restrict utterances to propns in that struc


; The following functions define alternative paradigmatic persons.  Differences are uppercased.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Canonical persons: They have all of the propositions defined above.
;; The two here differ in what they perceive.

; MAKE-KNOW-ALL-SEE-HUNTING
; Make a person that:
; Has propns about everything: parenting, hunting, earth origin, sky origin
; Initial salience on propns about: HUNTING
(defun make-know-all-see-hunting (name)
    (make-person name 'folks HUNTING-PROPNS
                 `((make-struc 'target 'problem '(start (,@generic-origin-propns ,@earth-origin-propns ,@sky-origin-propns)))
                   (make-struc 'source 'problem '(start (,@parenting-propns ,@hunting-propns)))
                   ,@semantic-relations)
                 `(,@pragmatic-relations)
                 '()))

; MAKE-KNOW-ALL-SEE-PARENTING
; Make a person that:
; Has propns about everything: parenting, hunting, earth origin, sky origin
; Initial salience on propns about: PARENTING
(defun make-know-all-see-parenting (name)
    (make-person name 'folks PARENTING-PROPNS
                 `((make-struc 'target 'problem '(start (,@generic-origin-propns ,@earth-origin-propns ,@sky-origin-propns)))
                   (make-struc 'source 'problem '(start (,@parenting-propns ,@hunting-propns)))
                   ,@semantic-relations)
                 `(,@pragmatic-relations)
                 '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THESE FIRST TWO functions make persons that start with origin beliefs that are in sync with what's salient.
;; i.e. make-skyless-person creates someone who emphasizes parenting, and has available earth-origin beliefs.
;; i.e. make-earthess-person creates someone who emphasizes hunting, and has available sky-origin beliefs.

; MAKE-SKYLESS-PERSON
; Make a person that:
; Has propns about: parenting, hunting, EARTH origin, and possibly one other propn
; Lacks: most propns about SKY origin
; Initial salience on propns about: PARENTING
(defun make-skyless-person (name &optional addl-target-propn)
  (let ((addl-target-propn-list (if addl-target-propn `(,addl-target-propn) nil)))  ; quick hack to make inserting one propn or nothing at all below simple
    (make-person name 'folks PARENTING-PROPNS
                 `((make-struc 'target 'problem '(start (,@generic-origin-propns ,@EARTH-ORIGIN-PROPNS ,@addl-target-propn-list)))
                   (make-struc 'source 'problem '(start (,@parenting-propns ,@hunting-propns)))
                   ,@semantic-relations)
                 `(,@pragmatic-relations)
                 '())))

; MAKE-EARTHLESS-PERSON
; Make a person that:
; Has propns about: parenting, hunting, SKY origin, and possibly one other propn
; Lacks: most propns about EARTH origin
; Initial salience on propns about: HUNTING
(defun make-earthless-person (name &optional addl-target-propn)
  (let ((addl-target-propn-list (if addl-target-propn `(,addl-target-propn) nil)))  ; quick hack to make inserting one propn or nothing at all below simple
    (make-person name 'folks HUNTING-PROPNS
                 `((make-struc 'target 'problem '(start (,@generic-origin-propns ,@SKY-ORIGIN-PROPNS ,@addl-target-propn-list)))
                   (make-struc 'source 'problem '(start (,@parenting-propns ,@hunting-propns)))
                   ,@semantic-relations)
                 `(,@pragmatic-relations)
                 '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THESE NEXT TWO FUNCTIONS MAKE "PARADOXICAL" PERSONS that start with origin beliefs that are out of sync with what's salient.
;; i.e. make-skyless-hunter creates someone who emphasizes hunting, and does not initially have available many (or any) sky-origin beliefs.
;; i.e. make-earthess-parent creates someone who emphasizes parenting, and does not have available many (or any) earth-origin beliefs.

; MAKE-SKYLESS-HUNTER
; Make a person that:
; Has propns about: parenting, hunting, EARTH origin, and possibly one other propn
; Lacks: most propns about SKY origin
; Initial salience on propns about: HUNTING
(defun make-skyless-hunter (name &optional addl-target-propn)
  (let ((addl-target-propn-list (if addl-target-propn `(,addl-target-propn) nil)))  ; quick hack to make inserting one propn or nothing at all below simple
    (make-person name 'folks HUNTING-PROPNS
                 `((make-struc 'target 'problem '(start (,@generic-origin-propns ,@EARTH-ORIGIN-PROPNS ,@addl-target-propn-list)))
                   (make-struc 'source 'problem '(start (,@parenting-propns ,@hunting-propns)))
                   ,@semantic-relations)
                 `(,@pragmatic-relations)
                 '())))

; MAKE-EARTHLESS-PARENT
; Make a person that:
; Has propns about: parenting, hunting, SKY origin, and possibly one other propn
; Lacks: most propns about EARTH origin
; Initial salience on propns about: PARENTING
(defun make-earthless-parent (name &optional addl-target-propn)
  (let ((addl-target-propn-list (if addl-target-propn `(,addl-target-propn) nil)))  ; quick hack to make inserting one propn or nothing at all below simple
    (make-person name 'folks PARENTING-PROPNS
                 `((make-struc 'target 'problem '(start (,@generic-origin-propns ,@SKY-ORIGIN-PROPNS ,@addl-target-propn-list)))
                   (make-struc 'source 'problem '(start (,@parenting-propns ,@hunting-propns)))
                   ,@semantic-relations)
                 `(,@pragmatic-relations)
                 '())))

; add salience for hunting propns to one person
(defun hunterize-person (person) (setf *the-person* person) (mapc #'perceived HUNTING-PROPNS))

; add salience for hunting propns to one person
(defun parentize-person (person) (setf *the-person* person) (mapc #'perceived PARENTING-PROPNS))

; add salience for parenting propns to one person
(defun deparentize-person (person) (setf *the-person* person) (mapc #'perceived-negation PARENTING-PROPNS))

; add salience for parenting propns to one person
(defun dehunterize-person (person) (setf *the-person* person) (mapc #'perceived-negation HUNTING-PROPNS))
