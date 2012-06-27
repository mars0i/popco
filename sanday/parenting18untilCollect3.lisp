; parenting18untilCollect1.lisp
; v.1 of code for multiple runs of models using parenting18 code that
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


; The following functions define two alternative paradigmatic persons.  Differences are uppercased.

; MAKE-SKYLESS-PERSON
; Make a person that:
; Has propns about: parenting, hunting, EARTH origin, and possibly one other propn
; Lacks: most propns about SKY origin
; Initial salience on propns about: EARTH origin
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
; Initial salience on propns about: SKY origin
(defun make-earthless-person (name &optional addl-target-propn)
  (let ((addl-target-propn-list (if addl-target-propn `(,addl-target-propn) nil)))  ; quick hack to make inserting one propn or nothing at all below simple
    (make-person name 'folks HUNTING-PROPNS
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


;; SKY-TO-EARTH-POP-NO-NEG 
;; Start with persons who know sky origin, parenting, hunting, and possibly one earth origin propn.
;; Start with salience on hunting.
;; Once someone collects all of the earth origin propns, drop all salience, give num-to-flip parenting salience.
(defun sky-to-earth-no-neg (num-extra-persons addl-ticks num-to-flip output-basename &optional (do-drop-salience t))
  (collect-and-continue-run 
    #'make-earthless-person earth-origin-propns num-extra-persons 20 addl-ticks num-to-flip #'parentize-person nil output-basename do-drop-salience))

;; SKY-TO-EARTH-POP-ADD-NEG 
;; Start with persons who know sky origin, parenting, hunting, and possibly one earth origin propn.
;; Start with salience on hunting.
;; Once someone collects all of the earth origin propns, drop all salience, give num-to-flip parenting salience and anti-hunting salience.
(defun sky-to-earth-add-neg (num-extra-persons addl-ticks num-to-flip output-basename &optional (do-drop-salience t))
  (collect-and-continue-run 
    #'make-earthless-person earth-origin-propns num-extra-persons 20 addl-ticks num-to-flip #'parentize-person #'dehunterize-person output-basename do-drop-salience))

;; EARTH-TO-SKY-POP-NO-NEG 
;; Start with persons who know earth origin, parenting, hunting, and possibly one sky origin propn.
;; Start with salience on parenting.
;; Once someone collects all of the sky origin propns, drop all salience, give num-to-flip hunting salience.
(defun earth-to-sky-no-neg (num-extra-persons addl-ticks num-to-flip output-basename &optional (do-drop-salience t))
  (collect-and-continue-run 
    #'make-skyless-person sky-origin-propns num-extra-persons 20 addl-ticks num-to-flip #'hunterize-person nil output-basename do-drop-salience))

;; EARTH-TO-SKY-POP-ADD-NEG 
;; Start with persons who know earth origin, parenting, hunting, and possibly one sky origin propn.
;; Start with salience on parenting.
;; Once someone collects all of the sky origin propns, drop all salience, give num-to-flip hunting salience and anti-parenting salience.
(defun earth-to-sky-add-neg (num-extra-persons addl-ticks num-to-flip output-basename &optional (do-drop-salience t))
  (collect-and-continue-run 
    #'make-skyless-person sky-origin-propns num-extra-persons 20 addl-ticks num-to-flip #'hunterize-person #'deparentize-person output-basename do-drop-salience))

;; COLLECT-AND-CONTINUE-RUN 
;; Create persons with make-person-fn, making at least as many as there are propositions in 
;; propns-to-distrib, each getting one of those propns.  Make num-extra-persons additional
;; persons without any extra propositions from propns-to-distrib, also with make-person-fn.
;; Run until at least one person has all of the propns in propns-to-distrib.
;; Then drop salience and add back salience to num-to-flip randomly chosen persons using 
;; flip-fn (required), as well as anti-flip-fn if not nil.  See functions above for illustrations.
;; NetLogo and csv data will be stored in filenames constructed from output-basename.  
(defun collect-and-continue-run (make-person-fn propns-to-distrib num-extra-persons burn-in-ticks addl-ticks num-to-flip flip-fn anti-flip-fn output-basename &optional (do-drop-salience t))
  (setf *netlogo-output-name* (concatenate 'string "../data/" output-basename "NetLogoData.txt"))
  (setf *propns-csv-output-name* (concatenate 'string "../data/" output-basename "PropnData.csv"))
  (setf *random-state-file* (concatenate 'string "../data/" output-basename "RandomState.lisp"))
  (setf *time-runs* t)
  (setf *do-report-to-netlogo* t)
  (setf *do-report-propns-to-csv* t)

  ; make earth-origin persons that also each have a distinct member of sky-origin-propns
  (make-persons-with-addl-propn make-person-fn 'x propns-to-distrib) ; ["x" for has extra propn]

  ; if requested, make additional earth-origin persons with none of the sky-origin-propns
  (when (> num-extra-persons 0)
    (funcall make-person-fn 'temp-person) ; template for the additional persons
    (n-persons-with-name 'temp-person 'n num-extra-persons) ; ["n" for relatively naive--no extra propns]
    (rem-elt-from-property 'temp-person 'folks 'members)) ; abandon temp-person to the garbage collector ...

  ; the status messages are for the NetLogo file, which might not actually get used
  (set-status-message "-> letting pop stabilize before conversation <-\\n   sky propns spread through pop until collected   \\n   env switches from parenting to some hunting")
  
  ; now that pop is set up, we can start the run
  (init-pop)
  (print (get 'folks 'members))

  ; population "burn-in": bring population to a stable state before allowing conversation to start
  (setf *max-pop-ticks* burn-in-ticks)
  (setf *do-converse* nil)
  (popco) ; initialize output files, and then allow initial settling of the culture before conversation begins
  (setf *do-converse* t) ; after this we allow conversation

  (set-status-message "   letting pop stabilize before conversation   \\n-> sky propns spread through pop until collected <-\\n   env switches from parenting to some hunting")

  (let ((propns-to-distrib-syms (mapcar #'last-element propns-to-distrib))) ; get proposition names

    ; run until we have at least one member who's collected all of the sky propns, checking every 10 ticks.  (The 1000 is just a failsafe max stopping point.)
    (popco-until 10 #'(lambda () (find-member-with-propns-in-struc? 'target propns-to-distrib-syms)) 1000)  

    (let ((first-to-collect (find-member-with-propns-in-struc? 'target propns-to-distrib-syms))) ; store the name of the lucky individual was
      (set-status-message 
        (format nil "   sky propns filter through pop until collected in one person [~S at tick ~S]\\n-> drop parenting, added hunting for several <-"
                first-to-collect *pop-tick*))))

  ; Now we switch the environment
  (when do-drop-salience
    (drop-salience)) ; remove salience from all propns in all persons

  (let ((to-flip (random-subset num-to-flip (get *the-population* 'members))))
    (format t "Flipping ~S~%" to-flip)
    (mapc flip-fn to-flip)
    (when anti-flip-fn
      (mapc anti-flip-fn to-flip)))

  (popco-plus-t addl-ticks) 

) ; end of collect-and-continue-run
