;;parenting18analogsAsBiases1.lisp
; based originally on parenting18untilCollect5.lisp

;. experiments on influence of analogy on long-term persistence and
;  transmission. (Have I already done some of these, in effect?)
;  	[Well in the 7/2012 paper I had the perceived domains driving
;	the origin domains (with some cross-domain effects as well).
;	e.g. pegging the hunting propns raises the sky propns.
;	Note though that these experiments had no restrictions on
;	domain of discourse--i.e. propns were from all four domains.
;	So I haven't done what's described below.  But I can do it by
;	specifying converse-strucs in the make-person calls.]
;
;  .. Compare two populations.  In both, only target (source) propns are uttered.
;     Maybe these are restricted to one domain.
;     In one, there is a corresponding analog for the propns.
;     In the other, there's not.
;     What's the difference?
;
;  .. Compare two populations.  In both, only target (source) propns are
;     uttered, from two domains.
;     The pops differ as to which source (target) analog they have.
;     If this changes what fixes in the target domain, it shows
;     the role of bg analogy, as opposed to innateness, in biasing
;     tran.



; many of these params may be overriden below
;*************************************
; These are probably just the defaults in variables.lisp:
(setf *propn-excit-weight* .2L0)
(setf *propn-inhib-weight* -.025L0)
(setf *trust* .05)
(setf *perceived-excit* .5) ; default link weight to salient for propositions perceived as true in env
; note setting of +max-weight+ in variables.lisp
;*************************************
; INITIAL SETTINGS
;(setf *time-runs* nil)
(setf *max-pop-ticks* 100)
(setf *do-converse* nil)
(setf *do-update-propn-nets* t)
(setf *do-report-to-netlogo* t)
(setf *do-report-propns-to-csv* t)
(setf *do-report-analogy-nets-to-guess* t)
(setf *sleep-delay* nil)           ; If non-nil, pause this many seconds between generations
(setf *silent-run?* t)             ; If nil, use Thagard-style verbose reporting to console
;*************************************

; These are output to NetLogo:
(setf *propn-category-prefixes* '("OE" "OS" "P" "H"))
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
(defvar origin-propn-syms (mapcar #'last-element origin-propns)) ; list of the propn names


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


(defvar lifestyle-propns (append parenting-propns hunting-propns))
(defvar lifestyle-propn-syms (mapcar #'last-element lifestyle-propns)) ; list of the propn names


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
    (semantic-iff 'os-Heavenly-God 'os-God-Creates-Mysteriously .1) ; we don't make the analogous connection with earth-god; she has more detailed relationships
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

; utters only source (lifestyle) propns, has sky origin propns
(defun make-sky-biased-lifestyle-talker (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start (,@sky-origin-propns)))
                   (make-struc 'source 'problem '(start (,@parenting-propns ,@hunting-propns)))
                   ,@semantic-relations)
                 `(,@pragmatic-relations)
                 '(source)))

; utters only source (lifestyle) propns, has earth origin propns
(defun make-earth-biased-lifestyle-talker (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start (,@earth-origin-propns)))
                   (make-struc 'source 'problem '(start (,@parenting-propns ,@hunting-propns)))
                   ,@semantic-relations)
                 `(,@pragmatic-relations)
                 '(source)))

; utters only source (lifestyle) propns, has no origin propns
(defun make-originless-lifestyle-talker (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start ()))
                   (make-struc 'source 'problem '(start (,@parenting-propns ,@hunting-propns)))
                   ,@semantic-relations)
                 `(,@pragmatic-relations)
                 '(source)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Now run setup a population and popco from prompt or from a separate file.
