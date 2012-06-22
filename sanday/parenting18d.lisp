; parenting18c.lisp
; based on uncc 2012 model, but using semantic-iff rather than symlink-if-units


; many of these params will be overrident in (sequence)

;*************************************
(setf *propn-excit-weight* .2L0)
(setf *propn-inhib-weight* -.025L0)
(setf *trust* .05)
(setf *perceived-excit* .5) ; default link weight to salient for propositions perceived as true in env
; note setting of +max-weight+ in variables.lisp
;*************************************

;*************************
; INITIAL SETTINGS
(setf *max-pop-ticks* 0)
(setf *do-converse* t)             ; Whether to send utterances between persons
(setf *do-update-propn-nets* t)    ; Whether to update propn constraints from propn map units
(setf *do-report-to-netlogo* t)  ; Whether to create file for input to NetLogo 
(setf *do-report-analogy-nets-to-guess* t)
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

(defvar sky-origin-propn-syms (mapcar #'last-element sky-origin-propns))

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

(defvar earth-origin-propn-syms (mapcar #'last-element earth-origin-propns))


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

(defvar hunting-propn-syms (mapcar #'last-element hunting-propns))


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

(defvar parenting-propn-syms (mapcar #'last-element parenting-propns))


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


; The following functions define two alternative paradigmatic persons.  Differences are uppercased.

; MAKE-SKYLESS-PERSON
; Make a person that:
;	Has propns about:
;		parenting
;		hunting
;		earth origin
;               possibly one other
;       Lacks:
;               most propns about sky origin
;	Initial salience on propns about:
;		earth origin
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
;	Has propns about:
;		parenting
;		hunting
;		sky origin
;               possibly one other
;       Lacks:
;               most propns about earth origin
;	Initial salience on propns about:
;		sky origin
(defun make-earthless-person (name &optional addl-target-propn)
  (let ((addl-target-propn-list (if addl-target-propn `(,addl-target-propn) nil)))  ; quick hack to make inserting one propn or nothing at all below simple
    (make-person name 'folks HUNTING-PROPNS
                 `((make-struc 'target 'problem '(start (,@generic-origin-propns ,@SKY-ORIGIN-PROPNS ,@addl-target-propn-list)))
                   (make-struc 'source 'problem '(start (,@parenting-propns ,@hunting-propns)))
                   ,@semantic-relations)
                 `(,@pragmatic-relations)
                 '())))

; MAKE-PERSONS-WITH-ADDL-PROPN 
; Make as many persons as there are propns in addl-target-propns, adding one such propn to each person.
; Persons will be named basename+incremented-number.
; Arguments:
;       person-maker: function that will create a person from a person name and an additional 
;                     proposition to be added to whatever the function puts in target analog struc by default.
;       basename: base name of persons to be made
;       addl-target-propns: a list of additional propositions to be added to each new person's target analog struc
;       first-number [optional--defaults to 1]: start of numbers to add to basename
;
; Example usage:
;       (make-persons-with-addl-propn #'make-skyless-person 'e sky-origin-propns)
;       (make-persons-with-addl-propn #'make-earthless-person 's earth-origin-propns)
;
(defun make-persons-with-addl-propn (person-maker basename addl-target-propns &optional (first-number 1))
  (let ((names 
          (make-names2 basename first-number (length addl-target-propns)))) ; make one person name for each addl propn to be added
    (mapc person-maker names addl-target-propns)))


; POP-HAS-MEMBER-WITH-THESE-PROPNS-IN-STRUC? 
; Test for the existence of a person with all members of a set of propns.
; Note: Propositions are proposition symbols, NOT the messages containing predicate, arguments, and name.
(defun find-member-with-propns-in-struc? (generic-struc propns &optional (population *the-population*))
  (find-if #'(lambda (pers) (person-struc-has-these-propns? pers generic-struc propns))
           (get population 'members)))

(defun person-struc-has-these-propns? (person generic-struc propns)
  (struc-has-these-propns? (generic-to-personal-sym generic-struc person)
                           (mapcar #'(lambda (p) (generic-to-personal-sym p person))
                                   propns)))

(defun struc-has-these-propns? (personal-struc propns)
  (subsetp propns (get personal-struc 'propositions)))

; add salience for hunting propns to one person
(defun hunterize-person (person) (setf *the-person* person) (mapc #'perceived HUNTING-PROPNS))

; add salience for hunting propns to one person
(defun parentize-person (person) (setf *the-person* person) (mapc #'perceived PARENTING-PROPNS))

; add salience for parenting propns to one person
(defun deparentize-person (person) (setf *the-person* person) (mapc #'perceived-negation PARENTING-PROPNS))

; add salience for parenting propns to one person
(defun dehunterize-person (person) (setf *the-person* person) (mapc #'perceived-negation HUNTING-PROPNS))


; TEST RUN
(setf *time-runs* nil)
(make-persons-with-addl-propn #'make-skyless-person 'e sky-origin-propns) ; give each individual a distinct member of the sky-origin-propns
(init-pop)
(print (get 'folks 'members))
(setf *max-pop-ticks* 0)
(popco) ; initialize output files, etc.
(setf *time-runs* nil)

(set-status-message "-> sky propns spread through pop until collected <- \\n   env switches from parenting to some hunting")
(popco-until 10 #'(lambda () (find-member-with-propns-in-struc? 'target sky-origin-propn-syms)) 1000)  

(defvar sky-believer (find-member-with-propns-in-struc? 'target sky-origin-propn-syms))
(set-status-message 
  (format nil "   sky propns filter through pop until collected in one person [~S at tick ~S]\\n-> drop parenting, added hunting for several <-"
          sky-believer *pop-tick* sky-believer))
(drop-salience)
(do ((persons (get *the-population* 'members))
     (i 0 (1+ i)))
    ((>= i 5))
  (hunterize-person (elt persons i)))

(popco-plus-t 1000)


