; parenting18a.lisp
; based on uncc 2012 model, but using semantic-iff rather than symlink-if-units


; many of these params will be overrident in (sequence)

(defvar *pop-size* 4) ; may be altered by read below
(defvar *fraction-to-parentize* .25)
(defvar *fraction-to-dehunterize* 0)

;*************************************
;(setf *propn-excit-weight* .2L0)
(setf *propn-inhib-weight* -.025L0)
(setf *trust* .05)
(setf *perceived-excit* .5) ; default link weight to salient for propositions perceived as true in env
; note setting of +max-weight+ in variables.lisp
;*************************************

;*************************
; INITIAL SETTINGS
(setf *max-pop-ticks* 10)
(setf *do-converse* t)             ; Whether to send utterances between persons
(setf *do-update-propn-nets* t)    ; Whether to update propn constraints from propn map units
(setf *do-report-to-netlogo* t)  ; Whether to create file for input to NetLogo 
(setf *do-report-analogy-nets-to-guess* nil)
(setf *sleep-delay* nil)           ; If non-nil, pause this many seconds between generations
(setf *silent-run?* t)             ; Whether Thagard-style verbose reporting to console
;*************************

(setf *propn-category-prefixes* '("OE" "OS" "P" "H"))
(setf *propn-category-descriptions* '("creator is of the earth" "creator is from the sky" "parenting is important" "hunting is important"))

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

(defvar earth-origin-propns-A
  '(
    ;(inside (human e-god) oe-Protohuman-Inside)
    (process-from-to (oe-Protohuman-Inside o-Human-Alive) oe-Human-From-Within-God)
    ;(causes (e-god oe-Human-From-Within-God) oe-God-Creates-Human-From-Within)
    (natural-process (oe-God-Creates-Human-From-Within) oe-God-Creates-Naturally)
    ;(helps (e-god human) oe-God-Helps-Human)
    (causes (nothing oe-God-Helps-Human) oe-God-Nurtures)
    ;(nothing (nothing) oe-Nothing) ; allows nothings in source and target to become related
    (close (e-god human) oe-Earthly-God)
   ))

(defvar earth-origin-propns-B
  '(
    (inside (human e-god) oe-Protohuman-Inside)
    ;(process-from-to (oe-Protohuman-Inside o-Human-Alive) oe-Human-From-Within-God)
    (causes (e-god oe-Human-From-Within-God) oe-God-Creates-Human-From-Within)
    ;(natural-process (oe-God-Creates-Human-From-Within) oe-God-Creates-Naturally)
    (helps (e-god human) oe-God-Helps-Human)
    ;(causes (nothing oe-God-Helps-Human) oe-God-Nurtures)
    (nothing (nothing) oe-Nothing) ; allows nothings in source and target to become related
    ;(close (e-god human) oe-Earthly-God)
   ))

(defvar earth-origin-propns (append earth-origin-propns-A earth-origin-propns-B))

(defvar origin-propns (append generic-origin-propns sky-origin-propns earth-origin-propns))

; Hunting involves emotional/metaphorical paradox [problematic for ACME]: 
; Man and animal are both predator and prey; animal is both life-taking and and life-giving.
; [Would it help to split into distinct analog structures?]
(defvar hunting-propns
  '(
    ; parallel to creation? This is a kludgey symbolism hack.  Note it will be "observed" in "environment".
    (feels-power (man) h-Man-Power) ; man feels power/powerful/successful/able control nature--something.
    (power-source (game h-Man-Power) h-Game-Power-Source) ; game is the source of man's power
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

(defvar parenting-propns
  '(
    (alive (child) p-Child-Alive)
    ;; social location:
    (intimate-agent (woman child) p-Child-Close)
    ;; birth/creation:
    (inside (child woman) p-Protochild-Inside) ; (not a comment on abortion :-)
    (process-from-to (p-Protochild-Inside p-Child-Alive) p-Child-From-Within-Woman)
    (creates (woman p-Child-From-Within-Woman) p-Woman-Creates-Child-From-Within)
    (natural-process (p-Woman-Creates-Child-From-Within) p-Woman-Creates-Naturally)
    ;; interaction:
    (helps (woman child) p-Woman-Helps-Child)
    (causes (nothing p-Woman-Helps-Child) p-Woman-Nurtures)
    (nothing (nothing) p-Nothing) ; allows nothings in source and target to become related
   ))


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
    (semantic-iff 'oe-God-Creates-Human-From-Within 'os-God-Creates-Human -.5)
   ))

    ;(similar 'close 'distant (* -1 *ident-weight*))                ; HAS NO EFFECT since both appear in only target analogs
    ;(similar 'distant-agent 'intimate-agent (* -1 *ident-weight*)) ; HAS NO EFFECT since both appear in only source analogs
    ;(similar 'woman 'man (* -1 *ident-weight*))                    ; HAS NO EFFECT since both appear in only source analogs

; Put PRESUMED and IMPORTANT calls here:
(defvar pragmatic-relations '())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARTIAL TARGET ANALOGS

; All of these agents start with salience on whatever hunting propns they have

; knows about everything, except is missing half of the earth origin propns
(make-person 'hunter-parent-A 'folks HUNTING-PROPNS
             `((make-struc 'target 'problem '(start (,@generic-origin-propns ,@sky-origin-propns ,@earth-origin-propns-A)))
               (make-struc 'source 'problem '(start (,@PARENTING-PROPNS ,@HUNTING-PROPNS)))
               ,@SEMANTIC-RELATIONS)
             `(,@pragmatic-relations)
             '())

; knows about everything, except is missing the other half of the earth origin propns
(make-person 'hunter-parent-B 'folks HUNTING-PROPNS
             `((make-struc 'target 'problem '(start (,@generic-origin-propns ,@sky-origin-propns ,@earth-origin-propns-B)))
               (make-struc 'source 'problem '(start (,@PARENTING-PROPNS ,@HUNTING-PROPNS)))
               ,@SEMANTIC-RELATIONS)
             `(,@pragmatic-relations)
             '())

; knows nothing about earth god or parenting
(make-person 'sky-hunter 'folks HUNTING-PROPNS
             `((make-struc 'target 'problem '(start (,@generic-origin-propns ,@sky-origin-propns)))
               (make-struc 'source 'problem '(start (,@HUNTING-PROPNS)))
               ,@SEMANTIC-RELATIONS)
             `(,@pragmatic-relations)
             '())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; function that is supposed to switch direction of population
; does it by desalientizing everyone, then adding new members with opposite view.

; make nothing salient for anyone:
(defun drop-salience ()
  (remove-all-constraints-from 'salient)
  ; THIS SHOULD MAYBE BE DONE WITH CLEAR-PROPN IN acme.lisp:
  (mapc #'(lambda (person) (setf (get (get person 'env) 'all-propositions) '()))
        (get *the-population* 'members)))

; add salience for hunting propns to one person
(defun hunterize-person (person)
  (setf *the-person* person)
  (mapc #'perceived HUNTING-PROPNS))

; add salience for hunting propns to one person
(defun parentize-person (person)
  (setf *the-person* person)
  (mapc #'perceived PARENTING-PROPNS))

; add salience for parenting propns to one person
(defun deparentize-person (person)
  (setf *the-person* person)
  (mapc #'perceived-negation PARENTING-PROPNS))

; add salience for parenting propns to one person
(defun dehunterize-person (person)
  (setf *the-person* person)
  (mapc #'perceived-negation HUNTING-PROPNS))

; add salience for parenting propns to one person
; DOESN'T WORK AS OF 5/27/12 because PERCEIVED in popco.lisp always puts propns in an env's source struc,
; even when they are from the target struc.  This causes the propns to then be transmitted into the source
; struc, mixing target propns in the person's source struc.  This causes mappings from a propn to itself,
; which is not allowed.
;(defun deskyize-person (person)
;  (setf *the-person* person)
;  (mapc #'perceived-negation sky-origin-propns))

; add salience for hunting propns for the first n members of the pop [i.e. first in the member list]
(defun hunterize-n-persons (n)
  (mapc #'hunterize-person (first-n-persons n)))

; add negative salience for parenting propns for the last n members of the pop [i.e. last in the member list]
(defun deparentize-n-persons (n)
  (mapc #'deparentize-person (last-n-persons n)))

; add salience for hunting propns for the first n members of the pop [i.e. first in the member list]
(defun parentize-n-persons (n)
  (mapc #'parentize-person (first-n-persons n)))

; add negative salience for parenting propns for the last n members of the pop [i.e. last in the member list]
(defun dehunterize-n-persons (n)
  (mapc #'dehunterize-person (last-n-persons n)))

(defun switch-pop ()
  (let ((pop-size (length (get *the-population* 'members))))
    (drop-salience)
    ; now make hunting salient for some:
    (parentize-n-persons (round (* pop-size *fraction-to-parentize*)))
    (dehunterize-n-persons (round (* pop-size *fraction-to-dehunterize*)))
    ; now start people talking
    (setf *do-converse* t)))

(defvar basic-status-message 
"stabilize on hunting; no conversation
begin conversation
two drop salience, two switch to parenting")

(defun setup-pop ()
  (setf *time-runs* nil)
;  (setf *write-person-graphs-at-pop-ticks* '(25 200 500 1000 1100 1200 1500 2000 2100 2200 2500 3000))
;  (setf *person-graphs-basename* "graphs/sanday/parenting18/a/noanalogy")
  (n-persons-with-name 'sky-hunter 's 2)
  (rem-elt-from-property 'sky-hunter 'folks 'members) ; but then delete it--simply because its name is long
  (persons-like 'hunter-parent-A '(hA))
  (persons-like 'hunter-parent-B '(hB))
  (rem-elt-from-property 'hunter-parent-A 'folks 'members) ; but then delete it--simply because its name is long
  (rem-elt-from-property 'hunter-parent-B 'folks 'members) ; but then delete it--simply because its name is long
  (init-pop))

(defun make-initial-graphs ()
  (setf *do-report-analogy-nets-to-guess* t)
  (write-person-graphs "graphs/sanday/parenting18/a/analogy/0/" 'folks :include-graph-label-nodes nil)
  (setf *do-report-analogy-nets-to-guess* nil)
  (write-person-graphs "graphs/sanday/parenting18/a/noanalogy/0/" 'folks :include-graph-label-nodes nil))

(defun run-pop ()
  (time (progn
          (set-status-message 
"-> stabilize on hunting; no conversation <- [until tick 30]\\n   begin conversation   \\n   two drop salience, two switch to parenting")
          (setf *max-pop-ticks* 30)
          (setf *do-converse* nil)
          (popco)
          (set-status-message
"   stabilize on hunting; no conversation   \\n-> begin conversation <- [until tick 600]\\n   two drop salience, two switch to parenting")
          (setf *do-converse* t)
          (popco-plus-t 570)
          ;(setf *do-report-analogy-nets-to-guess* t)(write-person-graphs "graphs/sanday/parenting18/a/analogy/600/")
          ;(setf *do-report-analogy-nets-to-guess* nil)(write-person-graphs "graphs/sanday/parenting18/a/noanalogy/600/")
          (set-status-message
"   stabilize on hunting; no conversation   \\n   begin conversation   \\n-> two switch to parenting salience, two drop salience <- [until tick 3000]")
          (drop-salience)
          (dehunterize-person 'hA)
          (parentize-person 'hA)
          (dehunterize-person 'hB)
          (parentize-person 'hB)
          (popco-plus-t 1400)
          ;(setf *do-report-analogy-nets-to-guess* t)(write-person-graphs "graphs/sanday/parenting18/a/analogy/2000/")
          ;(setf *do-report-analogy-nets-to-guess* nil)(write-person-graphs "graphs/sanday/parenting18/a/noanalogy/2000/")
          (popco-plus-t 1000)
          ;(setf *do-report-analogy-nets-to-guess* t)(write-person-graphs "graphs/sanday/parenting18/a/analogy/3000/")
          ;(setf *do-report-analogy-nets-to-guess* nil)(write-person-graphs "graphs/sanday/parenting18/a/noanalogy/3000/")
        )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RUN IT:

;(prompt-for-params)
;(n-persons-with-name 'hunter-parent 'p *pop-size*) ; clone 'mless-hunter-parent
;(rem-elt-from-property 'hunter-parent 'folks 'members) ; but then delete it--simply because its name is long
;(print-parameters)
;(init-pop)
;(print (get 'folks 'members))

;(setup-pop)
