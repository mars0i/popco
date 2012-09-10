;; parenting18coherence1a.lisp
; this is parenting18coherence1.lisp with some experimental modifications
; diff the two files to see the difference.
;
; Goals:
;
; Illustrate coherence effects.
;
; e.g. show that coherence due to background analogy can work against
; believing a propn that otherwise would be consistent with other beliefs.
; i.e. let salience or conversation emphasize s1-sn, those create emphasis
; on t1-tm due to analogy, and this fact makes it hard to disbelieve s(n+1),
; despite that otherwise that would work.
;
; Or, first, show that you can get a positive degree of belief in a propn
; just from the fact that (a) others in the domain are believed, and
; (b) they all are part of an analogy to another domain (well actually,
; it's several analogies ...).  i.e. let the analogy pass activation from
; one belief to others.
;
; One strategy:
; 
; Give each individual all of the propns.
; 
; Make most source propns in one domain (parenting or hunting) salient for
; one individual, either by making them directly perceived, or by making
; the perceived for others, and then letting conversation emphasize these
; propns.
; 
; Make one proposition in the same domain disbelieved, either through
; direct perception or conversation, as before.
; 
; Choose this last proposition so that:
; 
; It has no or few positive links to propns in its own (source) domain.
; 
; It maps to propns which do have significant positive links to others in
; the analog (target) domain.
; 
; Show that activation flows from the emphasized source propns through the
; analogical connections to the target propns, and back to the chosen
; source propn.
; 
; Show that this propn will not go as negative as it would otherwise, and
; maybe even can be maintained as positive.
; 
;..........................
; A Simpler experiment:
; TODO:
; set as perceived all hunting propns except one of
;         h-Game-harms-man
;         h-Skillful-hunting
;         h-Game-provides
;         h-Man-endangers-game
; and see if the net will make the nonobserved one positive.
; or try observing all parenting, except for 
;         p-Protochild-inside 
;         p-Woman-helps-child
; These six are part of a richly connected but kind of indirect part of the network.
; 
; other similar experiments:
; observe all except one of
;         h-hunting-rewards-skill
;         h-hunting-is-dangerous
; This will clearly work since they are part of a 4-node alternativing positive/negative square.
; 
; I think ignore the subnet with nothings in it, because it's wierd--includes both parenting and hunting
; positively linked to o-human-alive.
; 
; I think, actually, any of the other parenting or hunting propns would work.  Could just delete
; them from perception one by one ....


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
;    (offends (human s-god) os-Human-Offends-God)
;    (harms (s-god human) os-God-Harms-Human) ; god sometimes punishes
;    (causes (os-Human-Offends-God os-God-Harms-Human) os-Offense-Causes-Punishment)
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
;    (hunts-endangers (man game) h-Man-Endangers-Game) ; one aspect of hunting
;    (harms (game man) h-Game-Harms-Man)
;    (causes (h-Man-Endangers-Game h-Game-Harms-Man) h-Hunting-Is-Dangerous)
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

(defun make-generic-person (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start (,@origin-propns)))
                   (make-struc 'source 'problem '(start (,@parenting-propns ,@hunting-propns)))
                   ,@semantic-relations)
                 `(,@pragmatic-relations)
                 '()))

(defun make-originless-person (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start ()))
                   (make-struc 'source 'problem '(start (,@parenting-propns ,@hunting-propns)))
                   ,@semantic-relations)
                 `(,@pragmatic-relations)
                 '()))

(defun make-persons-one-unperceived (make-fn basename propns)
  (mapc #'(lambda (propn)
            (make-person-one-unperceived make-fn
                                         basename 
                                         propn
                                         (remove propn propns))) ; default eq test works: elements are from same list
        propns))


(defun make-person-one-unperceived (make-fn basename unperceived-propn perceived-propns)
  (funcall make-fn (simple-catname basename "-" 'wout "-" (last-element unperceived-propn))
                     perceived-propns))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; don't move graph around in telguess:
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")

(setf *do-converse* nil)
(make-generic-person 'ba) ; "blind" person--no perceptions--but has analogies
(make-generic-person 'bn) ; "blind" person with no analogies
(make-persons-one-unperceived #'make-generic-person    'ha hunting-propns)   ; hunter with analogies: 
(make-persons-one-unperceived #'make-originless-person 'hn hunting-propns)   ; hunter with no analogies
(make-persons-one-unperceived #'make-generic-person    'pa parenting-propns) ; parent with analogies
(make-persons-one-unperceived #'make-originless-person 'pn parenting-propns) ; parent with no analogies
(init-pop)
(print (get 'folks 'members))


; Change this for different experiments:
(setf *persons-reporting-to-guess*
      '(ha-wout-h-skillful-hunting ha-wout-h-hunting-rewards-skill))

(telguesses "18/coh1a/an/0/")
;(sleep 10)
;(start-reports-to-guess)
;(popco-plus-t 2)
