;; popco.lisp
;; Copyright (c) 2012 by Marshall Abrams
;; May be distributed only with permission from the author.

;; This is toplevel code for POPCO: Population Coherence Dynamics
;; by Marshall Abrams, but most of the guts of POPCO, in other files, are by Paul Thagard and his collaborators on COHERE.
;; I've modified PT's files to allow persons to maintain separate nets, and added and modified various functions.
;; v1.1, fall 2011

;; TO USE
;; First load:
;; 	popco-start.lisp [or its abbrevation start.lisp]
;; which loads the framework.
;; Then load 
;;	some persons in a population (cf. testbush.lisp).
;; which should run
;; 	(create-nets population)
;; or
;; 	(init-pop population)
;; [which do the same thing] to initialize the coherence network in each person.
;; Then call 
;; 	one of the run-population functions below, or their popco abbreviations.
;; You might also want to change the global variables mentioned below.

;; NAMING CONVENTION:
;; Functions with plural nouns in their names generally do something with entire population or collection.  
;; Functions with singular nouns do things to individual persons, etc.

(defvar *personal-separator* "_")
;(defconstant *personal-separator* "_") ; makes SBCL error on reload

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VARIABLES TO CONTROL OVERALL OPERATION OF POPCO

(setf *max-pop-ticks* 30) ; max number of pop-tick iterations; 0 for infinite

(setf *max-times* 5) ; defined in variables-personal.lisp. Here means how many cycles of net-settling allowed per pop-tick.

(setf *pop-tick* 0) ; number/time of the tick currently being processed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Some Lisp's (e.g. SBCL) use the same random state every time, by
; default, causing conversation sequences to be identical.
; Get a seed from the operating system.
; (In SBCL on OSX, this new seed to be the result of a pretty good random number
; generator--see SBCL source file target-random.lisp and 'man urandom'):
(when *use-new-random-seed*
  (setf *random-state* (make-random-state t)))

(defvar *random-state-file* "data/popcoRandomState.lisp") ; we'll write code to restore this session's random state here

(defvar *netlogo-output-name* "data/popcodata.txt") ; filename for file-based output to external UI program
(defvar *netlogo-outstream*)                   ; stream for same

; SPECIAL is PT's all-purpose external-emphasis node, used to give extra activation
; to identical or semantically-related predicates, propositions presumed or desired
; to be true, etc.  It's set to 1.0 in various places, though set to another value
; one function.  Just to be sure, we'll set it to 1.0 here.
(setf (activation 'special) *special-activation*)

; SALIENT is just like the special node, used by e.g. popco functions such as 
; PERCEIVED to mark propositions as being the direct result of observation.  
; We could use special instead--makes no difference to functioning of the code--
; but this more easily makes graphical display of the coherence networks perspicuous
; by making it apparent that the proposition constraint nets are functionally disconnected
; from the ACME network.  [NOTE though that special is still used by similar functions e.g. in ECHO.]
(setf (activation 'salient) *special-activation*)

;; [POPCO is a convenience front-end/abbreviation for this function.]

(defvar *time-runs* t)

(defun popco (&key cont-prev-sess) 
  (format t "~%Running popco with maximum of ~S cycles each in ~S popco tick(s) ....~%" *max-times* (- *max-pop-ticks* *pop-tick*))
  (format t "*do-converse* = ~S; *do-update-propn-nets* = ~S; *do-report-to-netlogo* = ~S *use-new-random-seed* = ~S~%"
             *do-converse*        *do-update-propn-nets*      *do-report-to-netlogo*      *use-new-random-seed*)
  (if *time-runs* 
    (time (run-population *the-population* :cont-prev-sess cont-prev-sess))
    (run-population *the-population* :cont-prev-sess cont-prev-sess))) 

(defun popco-plus-t (addl-ticks)
  (setf *max-pop-ticks* (+ *pop-tick* addl-ticks))
  (popco :cont-prev-sess t))

(defun popco1 ()
  (popco-plus-t 1))

;; MAIN LOOP
;; RUN-POPULATION
;; Main loop through ticks/time, with inner loop through persons
;; Loosely inspired by Thagard's consensus functions in consensus.lisp.
;; ARGS:
;; Population is a Lisp symbol with a list of persons in its members field--
;; synonym for Thagard's group (see e.g. make-persons in PT's consensus.lisp)
;; OPTIONAL KEYWORD ARGS:
;; Specify cont-prev-session as non-nil to append to outfile; otherwise outfile will be deleted.

(defun run-population (population &key cont-prev-sess)
  ; If user doesn't request continue prev session, store current random seed/state into a file that can recreate it later:
  (unless cont-prev-sess
    (with-open-file (random-state-file-stream *random-state-file* :direction :output :if-exists :rename :if-does-not-exist :create)
      (format random-state-file-stream "(setf *random-state* ~S" *random-state*)))
  ; If user doesn't request continue prev session, then delete/move output file if exists, and create new file:
  (when (and *do-report-to-netlogo* (not cont-prev-sess))
    (format t "Recreating output file for NetLogo ~S~%" *netlogo-output-name*)
    (with-open-file (*netlogo-outstream* *netlogo-output-name* :direction :output :if-exists :rename :if-does-not-exist :create)
      (princ *netlogo-syntax-description* *netlogo-outstream*)
      (report-persons-initially population)))
  (do ()
    ((time-to-stop) population)
    (when *sleep-delay* (sleep *sleep-delay*))
    (if *do-report-to-netlogo*
      (with-open-file (*netlogo-outstream* *netlogo-output-name* :direction :output :if-exists :append :if-does-not-exist :create)
        (run-population-once population))
      (run-population-once population))
    ;(format t "~%here: ~S ~S~%" (car *write-person-graphs-at-pop-ticks*) *pop-tick*) ; DEBUG
    (when (and *write-person-graphs-at-pop-ticks* (= *pop-tick* (car *write-person-graphs-at-pop-ticks*)))
      ;(format t "~%found one! ~S~%" *pop-tick*) ; DEBUG
      (setf *write-person-graphs-at-pop-ticks* (cdr *write-person-graphs-at-pop-ticks*))
      (write-person-graphs (format nil "~A/~A/" *person-graphs-basename* *pop-tick*))))
  (when *do-report-to-netlogo*
    (with-open-file (*netlogo-outstream* *netlogo-output-name* :direction :output :if-exists :append :if-does-not-exist :create)))
  t)
; maybe these multiple open files can be cleaned up...

;; RUN-POPULATION-ONCE
;; Guts of the main loop.
;; Logic that's not obvious from structure of the code:
;; - Though main loop has functional form, it modifies population along the way.
;; - There might be one or more agents who represent the
;;   environment rather than persons, with different internals.  [not yet implemented]
;; - sex-and-death might cause beliefs of newborns to be partly initialized. [not yet implemented]
;; If called directly, output file will automatically be appended to [call del-popco-outfile to start fresh]

(defun run-population-once (population)  ; [input->output] for each stage is indicated in comments.
  (incf *pop-tick*)
  (update-proposition-nets                       ; update proposition network links based on activations of proposition-map-units [pop->pop]
    (update-analogy-nets                         ; update internal analogy nets to reflect newly added propositions [pop->pop]
      (report-conversations                      ; optionally report conversations to external gui, etc. [(conversations-plus . pop)->pop]
        (transmit-environments                   ; like transmit utterances, but "utterances" of the external world, or other "cognitively spontaneous" emphasis
          (transmit-utterances                   ; add propositions uttered to listeners [(conversations . pop)->(conversations-plus . pop)]
            (choose-utterances                   ; choose propositions spoken [(converser-pairs . pop)->(conversations . pop)]
              (choose-conversers                 ; choose who talks to whom [pop->(converser-pairs . pop)]
                (births-and-deaths               ; choose who lives, dies, has babies, and do it [pop->pop] [split?] [currently a NO-OP]
                  (report-persons                ; optionally report state of persons to external gui, etc. [pop->pop]
                    (settle-nets population)))))))))) ; settle nets, at least partially [pop->pop] +
  (report-progress-to-console) ; brief report on what tick [etc.] just occurred
  (finish-output))

; [Yes there's inefficiency in looping and consing about conversing and
; uttering multiple times.  maybe get rid of that later.  It's nice to
; be able to see the logic of the process at the top level, though.]

; Give person running the program a quick brief update so they know that something's happening.
; This version reports pop-tick number, in the same terminal location each time.
; i.e. prints pop-tick number on stdout, in place, i.e. backspacing over previous number.
; Assumes that terminal handles backspace normally, and it won't backspace to prev line. (e.g. not gui version of CCL)
; Also assumes less than 1 billion pop-ticks.
(defun report-progress-to-console ()
  (format t "~C~C~C~C~C~C~C~C~C~S" #\backspace #\backspace #\backspace #\backspace #\backspace #\backspace #\backspace #\backspace #\backspace
                                         *pop-tick*))

(defun del-popco-outfile ()
  (delete-file *netlogo-output-name*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MACROS
;; Moved to popco-macros.lisp

;(defmacro prop-of-conv (conversation) `(first ,conversation))
;(defmacro speaker-of-conv (conversation) `(second ,conversation))
;(defmacro listener-of-conv (conversation) `(third ,conversation))
;(defmacro analog-struc-of-propn (propn) `(caar (get ,propn 'belongs-to))) ; there's only one such [like struc-from-propn in acme.lisp]
;(defmacro analog-struc-of-pred (pred) `(car (get ,pred 'belongs-to)))
;(defmacro analog-struc-of-obj (obj) `(car (get ,obj 'belongs-to)))     ; BUG CURRENTLY OBJS DON'T HAVE THIS
;(defmacro speaker-of-cpair (conversepair) `(car ,conversepair))
;(defmacro listener-of-cpair (conversepair) `(cdr ,conversepair))
;(defmacro target-analog-strucs-of-person (person)
;  `(remove-if-not #'target-analog-struc?
;    (get ,person 'all-structures)))
;(defmacro source-analog-strucs-of-person (person)
;  `(remove-if #'target-analog-struc?
;    (get ,person 'all-structures)))
;(defmacro pop-members (pop) (get pop 'members))

; Overwrite definition in imp.lisp
; Better to set this in the model input file, if it's desired:
;(defmacro normalize-degree (degree) `(logistic ,degree))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions at top level of loop in run-population[-once] and their helpers

;;-----------------------------------------------------
;; TIME-TO-STOP

;; Check wether it's time to break out of main loop
(defun time-to-stop ()
  (or 
    (and (> *max-pop-ticks* 0)
         (>= *pop-tick* *max-pop-ticks*))
    (user-says-stop)))

;; Check whether use has indicated wants to interrupt the loop
;; might be made more complicated if needed for run-time interaction with user
(defun user-says-stop () *stop-run?*)


;;-----------------------------------------------------
;; CHOOSE-CONVERSEPAIRS

;; Produce a list of persons who are to converse (probably only one-way eg 
;; first talks to second) i.e. a list of conversers, each containing two 
;; persons from the pop where noone appears in more than one pair. 
;; [consensus.lisp's make-meeters isn't appropriate here]
;; ARGS:    population
;; RETURNS: a cons with a list of pairs (lists) of persons as car, 
;;          and the entire population as cdr
(defun choose-conversers (population) 
  (if (not *do-converse*)
    (cons nil population)
    (let* ((pop-members (get population 'members))
           (pop-size (length pop-members))
           (half-size (round (/ pop-size 2)))   ; round up if odd number of members
           (randomized-members (randomize pop-members))
           (subpop1-members (butlast randomized-members half-size)) ; If pop-size is odd, we left out the member in the middle of the randomized list.
           (subpop2-members (nthcdr half-size randomized-members))) ; That's OK; s/he'll just have to wait until next time.
      ;(unless *silent-run?*
      ;  (format t "pop-members: ~s~%" pop-members)
      ;  (format t "pop-size: ~s~%" pop-size)
      ;  (format t "half-size: ~s~%" half-size)
      ;  (format t "randomized-members: ~s~%" randomized-members)
      ;  (format t "subpop1-members: ~s~%" subpop1-members)
      ;  (format t "subpop2-members: ~s~%" subpop2-members))
      (cons
        (mapcar #'list subpop1-members subpop2-members) ; list of pairs
        population))))



;;-----------------------------------------------------
;; CHOOSE-UTTERANCES

;; ARG: A list whose car is a set of pairs of persons (the first of whom will speak to the other)
;;      and the population (not used--simply passed through for the sake of functional-ey-ness).
;; RETURNS: A similar list, but with an interpersonal proposition consed onto each pair of persons.
;;          This will be what the first person says to the second in each case.
;;          Pairs are random without replacement. 
;;          If population size is odd, one person will sit out this round.
;; NOTE: Currently tries to communicate from any analog in speaker.  Might want to restrict
;; this using *communicate-all-analogs*, defined above--see comment there.
(defun choose-utterances (conversers-and-pop)
  (cons 
    (mapcar #'choose-utterance (car conversers-and-pop))
    (cdr conversers-and-pop)))


;;(defun make-list (old-list)
;  (if (passes-test (car old-list))
;    (cons (do-something-to (car old-list))
;          (make-list (cdr old-list)))
;    (make-list (cdr old-list))))

(defun choose-utterance (conversepair)
  (let ((speaker (speaker-of-cpair conversepair)))
    (cons
      (choose-thought speaker)
      conversepair)))

; CHOOSE THOUGHT
; Helper function for choose-utterance.
; Currently randomly chooses a proposition from analog structures listed in
; converse-strucs, or from all-propositions if converse-strucs is nil.
; [Note this appends the proposition lists from structures in converse structs before
; making the random choice, rather than randomly choosing a structure first, so
; that all propositions in the specified structures get an equal chance.  So if
; what you want is for all structures to be used, specify converse-strucs as nil;
; this will avoid the expense of the append.]
; TODO Possibly add threshold, filtering out thoughts with activation near zero.
; [Question: Does that mean that nothing is said, or that the person chooses
; something else to say?  What if no proposition has a high enough activation?]
(defun choose-thought (speaker)
  (let* ((converse-strucs (get speaker 'converse-strucs))
         (thoughts (if converse-strucs
                     (apply #'append (mapcar #'propns-from-struc converse-strucs))
                     (get speaker 'all-propositions)))
         (intense-thoughts (remove-if #'activn-exceeds-threshold thoughts)))
    (if intense-thoughts
      (elt intense-thoughts (random (length intense-thoughts)))
      nil)))

(defun activn-exceeds-threshold (propn)
  (< (abs (get propn 'activation))
     *utterance-threshold*))

;;-----------------------------------------------------
;; TRANSMIT-UTTERANCES, TRANSMIT-ENVIRONMENT

(defun transmit-environments (conversations-plus-and-pop)
  (let ((*trust* 1.0)) ; will affect receive-utterance in call chain from transmit-utterance
    (mapc #'transmit-environment (get (cdr conversations-plus-and-pop) 'members)))
  conversations-plus-and-pop)

(defun transmit-environment (person)
  (mapc #'transmit-utterance (make-env-conversations person)))

(defun make-env-conversations (person)
  (let ((env (get person 'env)))
    (mapcar #'(lambda (propn) (list propn env person))
            (get env 'all-propositions))))

;; ARG: - car is a list of conversations; cdr is the population
;;      - a conversation is a list with a personal proposition first
;;        the speaker who's proposition it is second, and the intended
;;        listener third.
;; RETURNS: The list of conversations with added new-propn flag, and population 
;;        after communication has taken place, with internal networks possibly 
;;        altered, but without new network settling.
(defun transmit-utterances (conversations-and-pop)
  (cons
    (mapcar #'transmit-utterance (car conversations-and-pop))
    (cdr conversations-and-pop)))

;; TRUST-IN
;; How much does this listener trust this speaker? Currently returns constant value.
;; In theory could also be sensitive to topic of conversation, but note that
;; listener will evaluate proposition relative to existing beliefs regardless.
(defun trust-in (listener speaker) *trust*)

;; ARGS: - a conversation containing
;;           - a speaker's proposition to be transmitted
;;           - the speaker
;;           - the listener
;; RETURNS: - an extended conversation, with t consed on 
;;            to indicate it's a new addition to the listener, 
;;            or nil consed on to indicate it's not
(defun transmit-utterance (conversation)
  (when (get (second conversation) 'group) 
  (format t "transmitting utterance: ~S~%" conversation) ; DEBUG
  )
  (let ((speaker (speaker-of-conv conversation)))
    (setf *the-person* speaker)
    (let* ((listener (listener-of-conv conversation))
           (speaker-propn (prop-of-conv conversation))
           (generic-propn (personal-to-generic-sym speaker-propn)))
      (setf (activation generic-propn) (activation speaker-propn)) ; temporarily store activn in generic propn for communic to listener
      (list ; this list can provide information for displaying conversations in a gui
        ; next call adds proposition to listener if not already there, returning t if added, nil if not
	; note: changes *the-person*
        (receive-utterance generic-propn
                           (personal-to-generic-sym 
			     (analog-struc-of-propn speaker-propn))
                           listener
                           (trust-in listener speaker)) ; calculate trust here - receive-utterance doesn't know the speaker
                           
        generic-propn
        speaker
        listener))))

;; RECEIVE-UTTERANCE
;; ARGS: - a generic (interpersonal) proposition symbol.
;;       - the car of first item from the source personal proposition's belongs-to property:
;;          i.e. name of an analog struc
;;       - a person to whom this utterance should be sent
;;       - a degree of trust: number between 0 and 1 to weight the propn's transmitted credence;
;;         see utterance-influence to see what this really means.
;; RETURNS: The new-flag: t if the proposition is a new addition to listener's analog struc, nil if not
(defun receive-utterance (generic-propn generic-struc listener trust)
  (setf *the-person* listener)
  (let* ((personal-propn (generic-to-personal-sym generic-propn))
         (personal-struc (generic-to-personal-sym generic-struc))
         (is-new-thought (if (member personal-propn (get personal-struc 'propositions)) nil t))) ; the IF converts TRUE to T per se
    (setf (get listener 'settled?) nil) ; analogy net is unsettled by new propn, but propn net is unsettled by any utterance
    (when is-new-thought 
      (format t "new thought: ~S added to ~S~%" personal-propn personal-struc) ; DEBUG
      (add-to-struc personal-struc 'start (list (get generic-propn 'message))) ; this does nothing but set fields (also calls note-unit, but that's overrident in next line)
      (init-propn personal-propn *propn-init-activ*)
      (mark-propn-unit-newly-added personal-propn *the-person*)
      (invoke-semantic-iffs-for-propn personal-propn *the-person*))
    (update-salient-link personal-propn (utterance-influence generic-propn trust))
    is-new-thought))

;; UTTERANCE-INFLUENCE
;; This could be sensitive speaker's degree of confidence passed in 
;; generic propn's activation, but currently just checks its sign.
(defun utterance-influence (propn-from-speaker trust)
  (* trust
     (sign-of (activation propn-from-speaker))))

; UPDATE-SALIENT-LINK
; We need raw-make-symlink for this purpose because we want to sum 
; whether negative or positive; make-symlink won't sum into negative links.
(defun update-salient-link (propn weight)
  (when (unlinked? propn 'salient) ; if this link doesn't exist
    (mark-constraint-newly-added propn 'salient weight *the-person*)) ; record that we're making a new constraint, so popco can tell gui if desired
  (raw-make-symlink 'salient propn weight)) ; note this just calls make-link, which merely adds in weight if the link exists

;; INVOKE-SEMANTIC-IFFS
;; NOTE might need to add a max and min weight specification if these are summing with other iff sources such as analogical relationships.
(defun invoke-semantic-iffs-for-propn (personal-propn person)
  (format t "invoking semantic-iffs ~S~%" (find-semantic-iffs (get person 'semantic-iffs) personal-propn)) ; DEBUG
  (mapc #'apply-raw-make-symlink-if-units
        (find-semantic-iffs (get person 'semantic-iffs) personal-propn)))  ; note: find-semantic-iffs is in popco-utils.lisp

;(defun invoke-semantic-iffs-for-person (person)
;  (mapc #'apply-raw-make-symlink-if-units (get person 'semantic-iffs)))
;
;(defun invoke-semantic-iffs-for-pop (population)
;  (mapc #'invoke-semantic-iffs-for-person (get population 'members)))



;;-----------------------------------------------------
;; SETTLE-NETS

;; Settle internal network of each person until some perhaps
;; small number of cycles is reached, whether convegred or not.
;; ARGS:    a population
;; RETURNS: the same population, but after each person's network has settled
;;          for *max-times* cycles
(defun settle-nets (population)
  (mapc #'settle-net (get population 'members))
  population)

(defun settle-net (person)
  (setf *the-person* person)
  (run-hyp-net)) ; settle the network until asymptote or *max-times* cycles or interrupted

;;;;;;;; FUNCTIONS FOR COMMUNICATION WITH EXTERNAL APPLICATIONS ;;;;;;;;

;; report-conversations and report-persons are the top-level reporting functions,
;; along with report-persons-initially.

;; NOTE formatting functions now moved to popco-fmt.lisp

;; REPORT-CONVERSATIONS
;; Outputs data on conversations for use by an external GUI etc.
;; ARG: conversations/population pair produced by transmit-utterances.
;;      i.e. the conversations have added new-propn flags.
;; RETURNS: the population, unchanged
(defun report-conversations (conversations-plus-and-pop)
  (when *do-report-to-netlogo* 
    (if *do-converse*
      (princ (fmt-conversations-for-netlogo (car conversations-plus-and-pop)) *netlogo-outstream*)
      (princ "[]" *netlogo-outstream*))
    (terpri *netlogo-outstream*))
  (cdr conversations-plus-and-pop))

;; REPORT-PERSONS-INITIALLY
;; Currently only do NetLogo at the beginning, not eg GUESS, which I'm init'ing by hand outside of this code
(defun report-persons-initially (population)
  (when *do-report-to-netlogo* 
    (report-propn-categories-to-netlogo)
    (report-population-to-netlogo population)))

;; REPORT-PERSONS
;; ARG: a population
;; RETURNS: The same population, unchanged
(defun report-persons (population)
  (when *persons-reporting-to-guess*
    (report-persons-to-guess *persons-reporting-to-guess*))
  (when *do-report-to-netlogo* 
    (when *netlogo-status-message* 
      (report-status-to-netlogo)
      (setf *netlogo-status-message* nil))
    (report-population-to-netlogo population))
  (mark-persons-items-old population) ; since done reporting, no longer need to know what's new, so make it old
  population)

(defun set-status-message (msg)
  (setf *netlogo-status-message* msg))

; TODO: Reorganize the code in popco-fmt-guessCmds.lisp so that I can
; mapc through persons all at once, instead of individually for each
; kind of operation:
(defun report-persons-to-guess (persons)
  ; nodes:
  (add-guess-nodes-for-persons persons)
  (update-guess-nodes-for-persons persons)
  ; edges:
  (remove-guess-edges-for-persons persons)
  (add-guess-edges-for-persons persons)
  (update-guess-edges-for-persons persons)
  ; global:
  (update-guess-meta-for-persons persons))

(defun report-propn-categories-to-netlogo ()
  (princ (fmt-tree-for-netlogo *propn-category-prefixes*) *netlogo-outstream*)
  (princ (fmt-tree-for-netlogo *propn-category-descriptions*) *netlogo-outstream*)
  (terpri *netlogo-outstream*))

(defun report-status-to-netlogo ()
  (princ (format-netlogo-status-message) *netlogo-outstream*))

(defun report-population-to-netlogo (population)
    (princ 
      (fmt-tree-for-netlogo
        (mapcar #'fmt-person-for-netlogo (get population 'members)))
      *netlogo-outstream*)
    (terpri *netlogo-outstream*))

(defun mark-persons-items-old (population)
  (mapc #'mark-items-old (get population 'members)))

(defun mark-items-old (person)
  (mapc #'mark-unit-old (get person 'newly-added-propn-units)) ; undo what receive-utterance did
  (setf (get person 'newly-added-propn-units) nil)             ;  ditto
  (mapc #'mark-unit-old (get person 'newly-added-map-units))   ; undo what make-hyp-unit and make-obj-unit did
  (setf (get person 'newly-added-map-units) nil)               ;  ditto
  (setf (get person 'newly-added-constraints) nil)             ; undo what make-symlink and update-salient-link and maybe receive-utterance did
  (setf (get person 'newly-removed-constraints) nil))


;;-----------------------------------------------------
;; CREATE-NETS

; mnemonic synonym for create-nets
(defun init-pop (&optional (population *the-population*))
  (format t "~%Creating analogy networks for each person ....~%")
  (create-nets population))

(defun create-nets (population)
  (mapc #'create-net (get population 'members))
  (mark-persons-items-old population) ; previous step marked all items new, but we didn't need to know that.
  population)

;; Currently a person's input field should contain a list of Lisp code
;; which can cause a new coherence network to be constructed from scratch.
;; cf. person-coh in consensus.lisp
(defun create-net (person)
  (setf *the-person* person)
  (mapc #'eval (get person 'input))   ; run make-struc, semantic directives. make-struc stores into analog struc, stores into propns
  (mapc #'init-propn (get person 'all-propositions)) ; initialize propositions--even ones that won't end up being mapped
  ;old version: (mapc #'perceived (mapcar #'generic-to-personal-sym (get person 'given-el))) ; mark propns in given-el perceived.  [Or put calls into the input field via make-person.]
  (mapc #'perceived (get person 'given-el)) ; mark propns in given-el perceived.  [Or put calls into the input field via make-person.]
  (update-analogy-net person) ; create and record the net from what's been stored in person, analog strucs, propns
  (update-proposition-net person) ; initialize proposition net [won't reflect code in next lines, but needed so make-symlinks there have something to attach to]
  (mapc #'apply-raw-make-symlink-if-units (get person 'semantic-iffs))
  (mapc #'eval (get person 'addl-input))) ; [PROBABLY BUGGY--SHOULDN'T IT RUN IN LATER TICKS, TOO?] additional code, such as pragmatics directives, that needs to run after the main net is created

;;-----------------------------------------------------

; single-arg wrapper for note-unit for mapc'ing:
(defun init-propn (propn &optional (init-activ *propn-init-activ*) (person *the-person*))
  (note-unit propn init-activ person))

;;-----------------------------------------------------
;; UPDATE-NETS
;; Update structure of networks in each person to reflect any new propositions,

;; CURRENTLY ASSUMES THAT THERE IS EXACTLY ONE TARGET ANALOG STRUCTURE,
;; NAMED <PERSON>_TARGET (SPECIFIED BY 'TARGET IN MAKE-PERSON).
;; THERE CAN BE ANY NUMBER OF SOURCE ANALOG STRUCTURES, WHICH CAN
;; NAMED ANYTHING AS LONG AS IT'S NOT "TARGET".

;; Currently we tell whether an analog is target or source by whether it's called target--i.e. there can be only one target analog
;; *THE-PERSON* MUST BE SET PROPERLY
(defun target-analog-struc? (sym)
  (eq 'target (personal-to-generic-sym sym)))

(defun update-analogy-nets (population)
  (mapc #'update-analogy-net (get population 'members))
  population)

; UPDATE-NET: update internal network(s) of given person.
; constraint-map from acme.lisp does most of the real work here
(defun update-analogy-net (person)
  (setf *the-person* person) ; [redund when called from create-net]
  (mapc (lambda (targ)     ; applies constraint-map to all pairs of target, source analogs
          (mapc (lambda (src) (constraint-map targ src))
                (source-analog-strucs-of-person person)))  ; note *the-person* has to be set properly
        (target-analog-strucs-of-person person))           ;  even though person is passed in here
  (mapc #'mark-dont-sum-weights (get person 'newly-added-map-units)))  ; Added by MA 12/2011
  ; EXPLANATION OF MARKING: 
  ; 1. Map units are marked as new when created, in acme.lisp.
  ; 2. New-ness is recorded both in the 'newly-added-map-units list of the person
  ; for efficient treatment of all of them at once, and in the is-newly-added property 
  ; of the unit, for efficient treatment of units on their own.
  ; 3. Units should be marked as new for the pop-tick after they're created,
  ; so that graphical display functions can tell output code that a new thingey
  ; has to be made in the graphical output.
  ; 4. When setting up links for new map units, we want
  ; to sum link weights for duped excitatory relationships.  However, we don't
  ; want to sum weights in subsequent pop-ticks when we call constraint map
  ; on the same pairs of nodes again.  It's only when one of the nodes is new, 
  ; due to conversation, that weights should be summed during the initial
  ;  network construction process involving that node.  [make-symlink in
  ; network.lisp tests for the dont-sum-weights flag on both map units
  ; and will leave the link alone if either it's set for either unit.]
  ; Therefore we set the dont-sum-weights flag *after* calling constraint-map,
  ; but before the next iteration of conversation, which may result in new 
  ; unmarked map units.  [Note: No similar operation is needed on proposition-to-
  ; proposition links in the proposition network, since those link weights are 
  ; explicity overwritten each pop-tick from map unit activations. [THIS LAST
  ; COMMENT MAY BE INCORRECT AS OF 6/2012.]]

(defun mark-dont-sum-weights (unit)
  (setf (get unit 'dont-sum-weights) t))

; make/update proposition links from proposition-map-units
(defun update-proposition-nets (population)
  (mapc #'update-proposition-net (get population 'members))
  population)

; make/update proposition links from proposition-map-units
(defun update-proposition-net (person)
  (when *do-update-propn-nets*
    (setf *the-person* person) ; [redund if called from create-net]
    (mapc #'update-assoc-from-unit (get *the-person* 'propn-map-units)))) ; from acme-infer.lisp

; OLD CODE FORMERLY IN UPDATE-NET:
    ; old two-analog version of the constraint-map calls:
    ; (constraint-map (generic-to-personal-sym 'target)
    ;                 (generic-to-personal-sym 'source))

    ; The following code used to appear in update-analogy-net after the double mapc ... constraint-map ... form,
    ;  near the end of update-analogy-net, but it's only needed on demand, not in every pop-tick.
    ; [Taking it out makes a big difference in abcl--shaves off 1/3 of the time in one test--but doesn't
    ;  seem to matter for sbcl or ccl--Maybe they were compiling it away since nothing used it?]
    ;(setf *weight-of-all-constraints* (sum-constraints)) ; only used in coh-score; maybe delete
    ;(setf (get *the-person* 'all-constraints) 
    ;      (list-constraints (get *the-person* 'all-units))) ; useful to have record of constraints in person


; RECORD-CONSTRAINTS
; Sometimes useful to have list of constraints stored with each
; person, but, we don't need to maintain it on every iteration.
; This is a convenience function for user interaction, modeled
; on some lines in consensus.lisp.
; It uses list-constraints to run through all of a person's
; units, constructing representations of (symmetric) constraints 
; of the form (unit1 unit2 . weight), and storing them in the
; person's all-constraints property.
; NOTE that sum-constraints makes use of this as well.
; ******************************
; NOW MOVED TO popco-utils.lisp.
; ******************************

;;-----------------------------------------------------
;; POPCO-SPECIFIC LINK-MAKING FUNCTIONS

; Create an "agent" for a person which is a component of the environment--the world,
; and which will contain propositions.
; (Eventually persons might share these.)
(defun make-persons-env-sym (person)
  (read-from-string (concatenate 'string (symbol-name person) "-env")))

; PERCEIVED
; Perceive a propn.  Insert the proposition into a person's "external" environment, 
; setting activation to 1 by default.
; ARG: a proposition message, i.e. a full subject/predicate representation in list form.
; *THE-PERSON* MUST BE SET CORRECTLY.
(defun perceived (msg &optional (degree 1.0))
  (let* ((env (make-persons-env-sym *the-person*))
         (struc (generic-to-personal-sym 'source env)))
    (unless (get *the-person* 'env)           ; give the person its environment if doesn't exist
      (setf (get *the-person* 'env) env)
      (push struc (get env 'all-structures)))
    (make-propn struc 'ignored msg env)
    (init-propn (generic-to-personal-sym (last-element msg) env)
                degree env)))

; PERCEIVED-NEGATION
; Perceive a propn.  Insert the proposition into a person's "external" 
; environment, setting its activation to -1.
; ARG: a proposition message, i.e. a full subject/predicate representation in list form.
; *THE-PERSON* MUST BE SET CORRECTLY.
(defun perceived-negation (msg)
  (perceived msg -1))

; UNPERCEIVED
; Remove the property of being perceived from a proposition or its negation.  
; i.e. simply removes the proposition from the person's environment, whether its
; activation 1 or -1.
; (defun unperceived (propn)  )

; SEMANTIC-IFF, GENERIC-SEMANTIC-IFF
; [semantic-iff and generic-semantic-iff are two names for the same function.]
; Store a relationship which will cause/influence a link between two propns, adding in the specified weight.
; This is to replace calls to symlink-if-units in model specification files such as those in the sanday directory.
(defun generic-semantic-iff (generic-propn1 generic-propn2 weight &optional (person *the-person*))
  (personal-semantic-iff (generic-to-personal-sym generic-propn1)
                         (generic-to-personal-sym generic-propn2)
                         weight 
                         person))

; SEMANTIC-IFF
; This makes semantic-iff an abbreviation for generic-semantic-iff by setting its function-value to be the same function:
(setf (symbol-function 'semantic-iff) #'generic-semantic-iff)

; PERSONAL-SEMANTIC-IFF
; semantic-iffs will have the structure (propn1 propn2 weight)
; [so that we can just pass the entire list as is to apply].
(defun personal-semantic-iff (personal-propn1 personal-propn2 weight &optional (person *the-person*))
  (pushnew (list personal-propn1 personal-propn2 weight) (get person 'semantic-iffs)))


;; OBSOLETE--use semantic-iff:
;; MAKE-PERSONAL-SYMLINK-IF-UNITS
;; SYMLINK-IF-UNITS
;; Convenience function, especially for use in specification of a population.
;; Just calls make-symlink-if-units after personalizing generic unit names.
;; Allows forcing a link between two nodes within a person, without
;; having to embed generic-to-personal-sym's in top-level population code.
;; *THE PERSON* MUST BE SET PROPERLY.
(defun make-personal-symlink-if-units (generic-unit1 generic-unit2 weight)
  (make-symlink-if-units (generic-to-personal-sym generic-unit1)
                          (generic-to-personal-sym generic-unit2)
                          weight))

;; OBSOLETE--use semantic-iff:
;; Abbreviation for preceding, which see.
(defun symlink-if-units (generic-unit1 generic-unit2 weight)
  (make-personal-symlink-if-units generic-unit1 generic-unit2 weight))

;; OBSOLETE?--use semantic-iff:
;; MAKE-SYMLINK-IF-UNITS
;; Make symlink if the two units to be linked are suitable--if they
;; have activation values.  Silently returns nil if not.
;; Useful for specification of a population model,
;; to run the same function on everyone, but don't have it bomb if the
;; appropriate beliefs or other nodes don't exist.
(defun make-symlink-if-units (unit1 unit2 weight)
  (when (and (unit? unit1)
             (unit? unit2))
    (make-symlink unit1 unit2 weight))) ; from network.lisp


; RAW-MAKE-SYMLINK-IF-UNITS
;; Make symlink if the two units to be linked are suitable--if they
;; have activation values.  Silently returns nil if not.
(defun raw-make-symlink-if-units (unit1 unit2 weight)
  (when (and (unit? unit1)
             (unit? unit2))
    (raw-make-symlink unit1 unit2 weight))) ; from network.lisp

; APPLY-RAW-MAKE-SYMLINK-IF-UNITS
; Abbreviation for (apply #'raw-make-symlink-if-units lis) where lis is a list of arguments.
(defun apply-raw-make-symlink-if-units (unit1-unit2-weight-list)
  (apply #'raw-make-symlink-if-units unit1-unit2-weight-list))



;;-----------------------------------------------------
;; BIRTHS-AND-DEATHS

;; currently no-op

; NOTE:
; Currently the input property of a person functions as its genome
; (also possibly given-el).
; persons-like and n-persons in consensus.lisp copy this and
; almost no other properties.  Except they don't copy--they just
; reference.  i.e. the input properties of persons created with
; persons-like are EQ their model's input.  So implementing
; MUTATION WILL REQUIRE ACTUALLY COPYING THE INPUT PROPERTY
; e.g. with COPY-TREE--otherwise mutating e.g. a new offspring
; will affect all of its later cousins as well!

(defun births-and-deaths (population) population)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions

;; Convert a person-specific symbol name (e.g. for a proposition or predicate)
;; into a corresponding generic, i.e. not person-specific symbol.
;; Maybe this should be done with stored pointers instead.
; *THE-PERSON* MUST BE SET PROPERLY unless optional argument is used
(defun personal-to-generic-sym (sym &optional (person *the-person*))
  (if (member sym *special-units*) ; these are special units which should not be converted
    sym
    (let ((sym-name (symbol-name sym)))
      (if (not (search *personal-separator* sym-name))  ; note 0 is true
        (error "PERSONAL-TO-GENERIC-SYM: Trying to convert a generic symbol to a generic symbol: ~s" sym))
      (let ((person-tag (concatenate 'string (symbol-name person) *personal-separator*)))
        (if (eql 0 (search person-tag sym-name)) ; if sym-name begins w/ person-tag [rets nil if not found]
          (read-from-string (subseq sym-name (length person-tag))) ; get/make symbol without tag
          (error "PERSONAL-TO-GENERIC-SYM: Personal symbol ~s doesn't belong to person ~s." 
                 sym person))))))

;; Convert a generic symbol name (e.g. for a proposition or predicate)
;; into a corresponding persons-specific symbol.
;; NOTE current version doesn't check for whether there's also a person name embedded
;; in the symbol-name.
;; Maybe this should be done with stored pointers instead.
; *THE-PERSON* MUST BE SET PROPERLY unless optional argument is used
(defun generic-to-personal-sym (sym &optional (person *the-person*))
  (if (member sym *special-units*) ; these are special units which should not be converted
    sym
    (let ((sym-name (symbol-name sym)))
      (if (search *personal-separator* sym-name)  ; note 0 is true
        (error "GENERIC-TO-PERSONAL-SYM: Trying to convert a personal symbol to a personal symbol: ~s" sym))
      (read-from-string 
        (concatenate 'string (symbol-name person) 
                     *personal-separator*
                     sym-name)))))

; PERSONAL-SYM-P and GENERIC-SYM-P
; Unsophisticated tests for a symbol being a personalized symbol:
; personal-sym? just searches for the *personal-separator*, returning 
; a true valuei f so, nil otherwise.  Note simply returns the result
; of SEARCH, which could represent true as 0, for example.
; *THE-PERSON* MUST BE SET PROPERLY.
(defun personal-sym? (sym) 
  (search *personal-separator* (symbol-name sym)))

(defun generic-sym? (sym) 
  (not (personal-sym? sym)))

;; set properties in a person that used to be in globals (in variables.lisp)
(defun initialize-person-properties (person)
  (setf *the-person* person)
  (setf (get *the-person* 'all-constraints) nil)
  (setf (get *the-person* 'number-units) nil) ;"association list of numbers and unit names"
  (setf (get *the-person* 'total-units)  0); "length of (get *the-person* 'all-units)"
  (setf (get *the-person* 'settled?) nil); "Network has settled."
  (setf (get *the-person* 'all-structures) nil)
  (setf (get *the-person* 'all-concepts) nil)
  (setf (get *the-person* 'all-objects) nil)
  (setf (get *the-person* 'all-preds) nil)
  (setf (get *the-person* 'all-propositions) nil); "List of all propns."
  (setf (get *the-person* 'all-units) nil); "List of all units."
  (setf (get *the-person* 'asymptoted-units) nil); "Units that have reached asymptote."
  (setf (get *the-person* 'total-links) 0); "Total number of links created."
  (setf (get *the-person* 'total-times) 0); "Number of settle cycles that have been run."
  (setf (get *the-person* 'all-valence-units) nil); "List of units that have valences."
  (setf (get *the-person* 'evaluation-units) nil)) ; for HOTCO 2

;;-----------------------------------------------------
;; BIRTHS-AND-DEATHS

;; currently no-op

; NOTE:
; Currently the input property of a person functions as its genome
; (also possibly given-el).
; persons-like and n-persons in consensus.lisp copy this and
; almost no other properties.  Except they don't copy--they just
; reference.  i.e. the input properties of persons created with
; persons-like are EQ their model's input.  So implementing
; MUTATION WILL REQUIRE ACTUALLY COPYING THE INPUT PROPERTY
; e.g. with COPY-TREE--otherwise mutating e.g. a new offspring
; will affect all of its later cousins as well!

(defun births-and-deaths (population) population)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions

;; Convert a person-specific symbol name (e.g. for a proposition or predicate)
;; into a corresponding generic, i.e. not person-specific symbol.
;; Maybe this should be done with stored pointers instead.
; *THE-PERSON* MUST BE SET PROPERLY unless optional argument is used
(defun personal-to-generic-sym (sym &optional (person *the-person*))
  (if (member sym *special-units*) ; these are special units which should not be converted
    sym
    (let ((sym-name (symbol-name sym)))
      (if (not (search *personal-separator* sym-name))  ; note 0 is true
        (error "PERSONAL-TO-GENERIC-SYM: Trying to convert a generic symbol to a generic symbol: ~s" sym))
      (let ((person-tag (concatenate 'string (symbol-name person) *personal-separator*)))
        (if (eql 0 (search person-tag sym-name)) ; if sym-name begins w/ person-tag [rets nil if not found]
          (read-from-string (subseq sym-name (length person-tag))) ; get/make symbol without tag
          (error "PERSONAL-TO-GENERIC-SYM: Personal symbol ~s doesn't belong to person ~s." 
                 sym person))))))

;; Convert a generic symbol name (e.g. for a proposition or predicate)
;; into a corresponding persons-specific symbol.
;; NOTE current version doesn't check for whether there's also a person name embedded
;; in the symbol-name.
;; Maybe this should be done with stored pointers instead.
; *THE-PERSON* MUST BE SET PROPERLY unless optional argument is used
(defun generic-to-personal-sym (sym &optional (person *the-person*))
  (if (member sym *special-units*) ; these are special units which should not be converted
    sym
    (let ((sym-name (symbol-name sym)))
      (if (search *personal-separator* sym-name)  ; note 0 is true
        (error "GENERIC-TO-PERSONAL-SYM: Trying to convert a personal symbol to a personal symbol: ~s" sym))
      (read-from-string 
        (concatenate 'string (symbol-name person) 
                     *personal-separator*
                     sym-name)))))

; PERSONAL-SYM-P and GENERIC-SYM-P
; Unsophisticated tests for a symbol being a personalized symbol:
; personal-sym? just searches for the *personal-separator*, returning 
; a true valuei f so, nil otherwise.  Note simply returns the result
; of SEARCH, which could represent true as 0, for example.
; *THE-PERSON* MUST BE SET PROPERLY.
(defun personal-sym? (sym) 
  (search *personal-separator* (symbol-name sym)))

(defun generic-sym? (sym) 
  (not (personal-sym? sym)))

;; set properties in a person that used to be in globals (in variables.lisp)
(defun initialize-person-properties (person)
  (setf *the-person* person)
  (setf (get *the-person* 'all-constraints) nil)
  (setf (get *the-person* 'number-units) nil) ;"association list of numbers and unit names"
  (setf (get *the-person* 'total-units)  0); "length of (get *the-person* 'all-units)"
  (setf (get *the-person* 'settled?) nil); "Network has settled."
  (setf (get *the-person* 'all-structures) nil)
  (setf (get *the-person* 'all-concepts) nil)
  (setf (get *the-person* 'all-objects) nil)
  (setf (get *the-person* 'all-preds) nil)
  (setf (get *the-person* 'all-propositions) nil); "List of all propns."
  (setf (get *the-person* 'all-units) nil); "List of all units."
  (setf (get *the-person* 'asymptoted-units) nil); "Units that have reached asymptote."
  (setf (get *the-person* 'total-links) 0); "Total number of links created."
  (setf (get *the-person* 'total-times) 0); "Number of settle cycles that have been run."
  (setf (get *the-person* 'all-valence-units) nil); "List of units that have valences."
  (setf (get *the-person* 'evaluation-units) nil)) ; for HOTCO 2
