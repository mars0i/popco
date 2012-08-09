; parenting18untilCollect4.lisp
; v.4 of code for multiple runs of models using parenting18 code that
; runs until all parts of an origin domain are collected in one individual,
; and then possibly change salience relations.
; Based on parenting18d.lisp


; many of these params will be overrident in (sequence)

;*************************************
; These are probably just the defaults in variables.lisp:
(setf *propn-excit-weight* .2L0)
(setf *propn-inhib-weight* -.025L0)
(setf *trust* .05)
(setf *perceived-excit* .5) ; default link weight to salient for propositions perceived as true in env
; note setting of +max-weight+ in variables.lisp
;*************************************

;*************************
; INITIAL SETTINGS
;(setf *time-runs* nil)   ; set below
;(setf *max-pop-ticks* 0) ; set below
;(setf *do-converse* t)             ; Whether to send utterances between persons
(setf *do-update-propn-nets* t)    ; Whether to update propn constraints from propn map units
;(setf *do-report-to-netlogo* t)  ; Whether to create file for input to NetLogo 
;(setf *do-report-propns-to-csv* t)
(setf *do-report-analogy-nets-to-guess* nil)
(setf *sleep-delay* nil)           ; If non-nil, pause this many seconds between generations
(setf *silent-run?* t)             ; If nil, use Thagard-style verbose reporting to console
;*************************

; These are output to NetLogo:
(setf *propn-category-prefixes* '("OE" "OS" "P" "H"))
;(setf *propn-category-descriptions* '("creator is of the earth" "creator is from the sky" "parenting is important" "hunting is important"))
(setf *propn-category-descriptions* '("origin from female" "origin from animal" "parenting is important" "hunting is important"))

; overwrite definition in imp.lisp:
;(defmacro normalize-degree (degree) `(logistic ,degree))

;; first clear everything out
(mapcar #'clear-plists (get 'folks 'members))
(clear-person-nets 'folks)
(setf *the-population* 'folks)

;; CONVENTIONS:
;; Proposition names start with a prefix indicating domain, and are mixed case after that.
;; prefix key:
;;      o-  : generic origin proposition
;;	oe- : intended as earth/female origin symbol proposition
;;	os- : intended as sky/male origin symbol proposition
;;	h-  ; hunting subsistence propositions
;;      p-  ; propositions about parenting, childbirth
;;	g-  ; gathering subsistence propositions
;; All-uppercase can be used to flag things temporarily.

(defvar generic-origin-propns
  '(
    (alive (human) o-Human-Alive)
   ))

(defvar sky-origin-propns
  '(
    ;; creation:
    (creates (s-god o-Human-Alive) os-God-Creates-Human-From-Object) ; e-god's process is more complex
    (mysterious-process (os-God-Creates-Human) os-God-Creates-Mysteriously)
    ;; interaction:
    ;   punishment
    (offends (human s-god) os-Human-Offends-God)
    (harms (s-god human) os-God-Harms-Human) ; god sometimes punishes
    (causes (os-Human-Offends-God os-God-Harms-Human) os-Offense-Causes-Punishment)
    ;   reward
    (supplicates (human s-god) os-Human-Supplicates)
    (helps (s-god human) os-God-Helps-Human)
    (causes (os-Human-Supplicates os-God-Helps-Human) os-God-Rewards)
    ;; location:
    (distant (s-god human) os-Heavenly-God)
   ))
(defvar sky-origin-propn-syms (mapcar #'last-element sky-origin-propns)) ; list of the propn names

(defvar earth-origin-propns
  '(
    (inside (human e-god) oe-Protohuman-Inside)
    (process-from-to (oe-Protohuman-Inside o-Human-Alive) oe-Human-From-Within-God)
    (causes (e-god oe-Human-From-Within-God) oe-God-Creates-Human-From-Within)
    (natural-process (oe-God-Creates-Human-From-Within) oe-God-Creates-Naturally)
    (helps (e-god human) oe-God-Helps-Human)
    (causes (nothing oe-God-Helps-Human) oe-God-Nurtures)
    (nothing (nothing) oe-Nothing) ; allows nothings in source and target to become related
    (close (e-god human) oe-Earthly-God)
   ))
(defvar earth-origin-propn-syms (mapcar #'last-element earth-origin-propns)) ; list of the propn names

(defvar origin-propns (append generic-origin-propns sky-origin-propns earth-origin-propns))


; Hunting involves emotional/metaphorical paradox [problematic for ACME]: 
; Man and animal are both predator and prey; animal is both life-taking and and life-giving.
; There are a number of kludges here which distort in order to allow successful analogies.
(defvar hunting-propns
  '(
    (feels-power (man) h-Man-Power) ; man feels power/powerful/successful/able to control nature--something like that.
    (source-of (game h-Man-Power) h-Game-Power-Source) ; game is the source of man's power
    (mysterious-process (h-Game-Power-Source) h-Game-Power-Mysteriously) ; that game is source of man's power is mysterious
    ;; interaction:
    ; danger
    (hunts-endangers (man game) h-Man-Endangers-Game) ; one aspect of hunting
    (harms (game man) h-Game-Harms-Man)
    (causes (h-Man-Endangers-Game h-Game-Harms-Man) h-Hunting-Is-Dangerous)
    ; reward
    (hunts-skillfully (man game) h-Skillful-Hunting) ; another aspect of hunting
    (helps (game man) h-Game-Provides)
    (causes (h-Skillful-Hunting h-Game-Provides) h-Hunting-Rewards-Skill)

    ;; social location:
    (distant-agent (game man) h-Game-Distant) ; man treats game as agent--but as distant, as other
   ))
(defvar hunting-propn-syms (mapcar #'last-element hunting-propns)) ; list of the propn names


(defvar parenting-propns
  '(
    (alive (child) p-Child-Alive)
    ;; social location:
    (intimate-agent (woman child) p-Child-Close)
    ;; birth/creation:
    (inside (child woman) p-Protochild-Inside) ; not a comment on abortion, just a simplification
    (process-from-to (p-Protochild-Inside p-Child-Alive) p-Child-From-Within-Woman)
    (creates (woman p-Child-From-Within-Woman) p-Woman-Creates-Child-From-Within)
    (natural-process (p-Woman-Creates-Child-From-Within) p-Woman-Creates-Naturally)
    ;; interaction:
    (helps (woman child) p-Woman-Helps-Child)
    (causes (nothing p-Woman-Helps-Child) p-Woman-Nurtures)
    (nothing (nothing) p-Nothing) ; allows nothings in source and target to become related
   ))
(defvar parenting-propn-syms (mapcar #'last-element parenting-propns)) ; list of the propn names


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Put SIMILAR and SYMLINK-IF-UNITS calls here:

(defvar semantic-relations
  '(
    ; antonymic relationships:
    (similar 'helps 'harms (* -1 *ident-weight*))
    (similar 'feels-power 'alive (* -.75 *ident-weight*)) ; they're not antithetical, but not closely related
    ; similarity relationships, some which are kludged metaphors:
    (similar 'woman 'human (* .5 *ident-weight*))
    (similar 'man 'human (* .5 *ident-weight*))
    (similar 'distant 'distant-agent (* .5 *ident-weight*)) ; kludged metaphor [cf. Lakoff/Johnson]: physical and emotional distance are similar
    (similar 'close 'intimate-agent (* .5 *ident-weight*)) ; ditto
    (similar 'offends 'harms (* .5 *ident-weight*))
    ; within-analog antonymic relationships expressed via explicit symlinks on propns:
    (semantic-iff 'oe-Earthly-God 'os-Heavenly-God -.5) ; not contrad since e-god != s-god, but conflicting worldview
    (semantic-iff 'oe-God-Creates-Naturally 'os-God-Creates-Mysteriously -.5) ; not contrad since e-god != s-god, but conflicting worldview
    (semantic-iff 'os-Heavenly-God 'os-God-Creates-Mysteriously .1)
    ; KLUDGE: Why do I have to do the following explicitly? Shouldn't the analog stru do it?:
    (semantic-iff 'oe-God-Creates-Naturally 'oe-God-Creates-Human-From-Within .5)
    (semantic-iff 'oe-God-Nurtures 'oe-God-Creates-Human-From-Within .1)
    (semantic-iff 'oe-Human-From-Within-God 'oe-God-Creates-Human-From-Within .5)
    (semantic-iff 'oe-God-Creates-Human-From-Within 'os-God-Creates-Human-From-Object -.5)
   ))

    ;(similar 'close 'distant (* -1 *ident-weight*))                ; HAS NO EFFECT since both appear in only target analogs
    ;(similar 'distant-agent 'intimate-agent (* -1 *ident-weight*)) ; HAS NO EFFECT since both appear in only source analogs
    ;(similar 'woman 'man (* -1 *ident-weight*))                    ; HAS NO EFFECT since both appear in only source analogs

; Put PRESUMED and IMPORTANT calls here:
(defvar pragmatic-relations '())


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
