;; popco.lisp
;; Copyright (c) 2012 by Marshall Abrams
;; May be distributed only with permission from the author.

;; This is toplevel code for POPCO: Population Coherence Dynamics by Marshall Abrams
;; Many parts of POPCO, in other files, are by Paul Thagard and his collaborators on COHERE,
;; with extensive modifications in many areas.
;; e.g. I've modified PT's files to allow persons to maintain separate nets, and added and modified various functions.
;; -MA
;; v1.2, fall 2012

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

(setf *max-pop-ticks* 30) ; max number of pop-tick iterations

(setf *max-times* 5) ; defined in variables-personal.lisp. Here means how many cycles of net-settling allowed per pop-tick.

(setf *pop-tick* 0) ; number/time of the tick currently being processed.  
; Note this gets incremented before anything happens, so the first tick is actually = 1.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Call to MAKE-RANDOM-STATE now in variables.lisp, to precede setting *run-id*, which is used in other files.

(defvar *data-dir* "data")

(defvar *random-state-file* (format nil "~A/~A.lisp" *data-dir* *run-id*)) ; we'll write code to restore this session's random state here

(defvar *netlogo-basename* (format nil "~A/~A" *data-dir* *run-id*))
(defvar *netlogo-extension* ".nlogdat")  
(defvar *netlogo-output-name* (format nil "~A~A" *netlogo-basename* *netlogo-extension*)) ; main, ongoing NetLogo data file name
(defvar *netlogo-outstream* nil)    ; stream for same
(defvar *netlogo-snapshot-suffix* "AtTick") ; used in name of file for passing NetLogo pop state during a single tick; see report-persons-just-at-t-for-netlogo.

(defvar *propns-csv-output-name* (format nil "~A/~A.csv" *data-dir* *run-id*)) ; file to write propn activn data in csv format
(defvar *propns-csv-outstream* nil) ; stream for same

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

(defvar *time-runs* t)

; POPCO
; Run the main loop on *the-population* until *max-pop-ticks* is reached.
; If  ':cont-prev-sess t' is added, then don't recreate output files such 
; as the netlogo output file from scratch, but instead add to the existing file.
; Also displays basic information on nature of the run, and times it by default, i.e. if *time-runs* is true.
(defun popco (&key cont-prev-sess) 
  (format t "~%Running popco with maximum of ~S cycles each in ~S popco tick(s) ....~%" *max-times* (- *max-pop-ticks* *pop-tick*))
  (format t "*do-converse* ~S; *do-update-propn-nets* ~S; *do-report-to-netlogo* ~S; *do-report-propns-to-csv* ~S; *use-new-random-state* ~S~%"
             *do-converse*     *do-update-propn-nets*     *do-report-to-netlogo*     *do-report-propns-to-csv*      *use-new-random-state*)
  (if *time-runs* 
    (time (run-population *the-population* :cont-prev-sess cont-prev-sess))
    (run-population *the-population* :cont-prev-sess cont-prev-sess))) 

; POPCO-PLUS-T
; Run the main loop on *the-population*, starting from the current *pop-tick*, until
; addl-ticks more ticks are added.  Note ':cont-prev-sess t' will be passed to the popco command,
; so an existing data files such as the netlogo file will appended to, rather than recreated.
; This means that you should normally run (popco) first before running (popco-plus-t).
(defun popco-plus-t (addl-ticks)
  (setf *max-pop-ticks* (+ *pop-tick* addl-ticks))
  (popco :cont-prev-sess t))

; POPCO1
; Abbreviation for (popco-plus-t 1)
(defun popco1 ()
  (popco-plus-t 1))

; POPCO-UNTIL
; Run POPCO until test-function returns true, testing every how-often pop-ticks.
; optional arg stop-after-addl is the number of ticks after which to terminate if test still
; hasn't fired.  Note won't necessarily stop exactly at that tick, unless how-often = 1.
; Returns the elapsed real time in seconds.
; test-function takes no arguments, but obviously can reference globals such as *the-population*.
; Continues previous session, so (popco) should normally be run first.
; Note this function uses run-population directly to avoid the messages and time output that
; popco/popco-plus-t currently generate.
(defun popco-until (how-often test-function &optional stop-after-addl)
  (let ((stop-after (if stop-after-addl (+ *pop-tick* stop-after-addl) nil))
        (start-time (get-internal-real-time)))
    (do ()
        ((or (when stop-after              ; if stop-after was specified
               (>= *pop-tick* stop-after)) ; and pop-tick is stop-after or greater
             (funcall test-function)))     ; or test returns true, then terminate
      (setf *max-pop-ticks* (+ *pop-tick* how-often))      ; increment tick to run
      (run-population *the-population* :cont-prev-sess t)) ; and run until then

    ; now that we're done with the loop, tell user how long it ran:
    (let ((elapsed  (real-time-elapsed-since start-time)))
      (format t "~%~S seconds (~S minutes)~%" elapsed (/ elapsed 60.0))
      elapsed))) ; end of popco-until


;; MAIN LOOP
;; RUN-POPULATION
;; Main loop through ticks/time, with inner loop through persons
;; Most of the complication is to allow variation in how the data is reported.
;; Loosely inspired by Thagard's consensus functions in consensus.lisp.
;; ARGS:
;; Population is a Lisp symbol with a list of persons in its members field--
;; synonym for Thagard's group (see e.g. make-persons in PT's consensus.lisp)
;; OPTIONAL KEYWORD ARGS:
;; Specify cont-prev-session as non-nil to append to outfile; otherwise outfile will be deleted or renamed.

(defun run-population (population &key cont-prev-sess)
  (unwind-protect ; allows us to ensure that files will be closed even if an error occurs
    (progn
      ; what's inside this progn is the real work we want done

      ; If user doesn't request continue prev session, store current random seed/state into a file that can recreate it later:
      (unless cont-prev-sess
        (let ((*print-pretty* nil)) ; avoid padding with spaces--reduces file size by an order of magnitude in SBCL
          (with-open-file (random-state-file-stream *random-state-file* :direction :output :if-exists :rename :if-does-not-exist :create)
            (format random-state-file-stream "(format t \"~%Restoring previous random state from file.~%\")~%(setf *random-state* ~S)" *random-state*))))

      ; Now open files to record data as we go, if user specifies that we should.
      ; Whether we open the files for appending or for recreating depends on whether cont-prev-sess was specified as t, or is nil.

      (when *do-report-to-netlogo*
        (cond (cont-prev-sess
                (setf *netlogo-outstream* (open *netlogo-output-name* :direction :output :if-exists :append :if-does-not-exist :error)))
              (t
                (format t "Recreating output file for NetLogo ~S~%" *netlogo-output-name*)
                (setf *netlogo-outstream* (open *netlogo-output-name* :direction :output :if-exists :rename :if-does-not-exist :create))
                (report-persons-initially-for-netlogo population))))

      (when *do-report-propns-to-csv*
        (cond (cont-prev-sess
                (setf *propns-csv-outstream* (open *propns-csv-output-name* :direction :output :if-exists :append :if-does-not-exist :error)))
              (t
                (format t "Recreating output file csv file ~S~%" *propns-csv-output-name*)
                (setf *propns-csv-outstream* (open *propns-csv-output-name* :direction :output :if-exists :rename :if-does-not-exist :create))
                (report-persons-initially-for-csv population))))


      (do ()
          ((time-to-stop) 
           (do-final-reports population) population) ; keep looping until (time-to-stop) returns true
        (when *sleep-delay* (sleep *sleep-delay*))
        (run-population-once population)
        (when (and *write-person-graphs-at-pop-ticks* (= *pop-tick* (car *write-person-graphs-at-pop-ticks*)))
          (setf *write-person-graphs-at-pop-ticks* (cdr *write-person-graphs-at-pop-ticks*))
          (write-person-graphs (format nil "~A/~A/" *person-graphs-basename* *pop-tick*))))

     ) ; end of progn for main code in unwind-protect

    ; cleanup routines for unwind-protect
    (when *netlogo-outstream* (close *netlogo-outstream*))
    (when *propns-csv-outstream* (close *propns-csv-outstream*))
   ) ; end of unwind-protect
 t)
; end of run-population


;; RUN-POPULATION-ONCE
;; Guts of THE MAIN LOOP.
;; Logic that's not obvious from structure of the code:
;; - Though main loop has functional form, it modifies population along the way.
;; - There might be one or more agents who represent the
;;   environment rather than persons, with different internals.  [not yet implemented]
;; - sex-and-death might cause beliefs of newborns to be partly initialized. [not yet implemented]
;; If called directly, output file will automatically be appended to [call del-netlogo-outfile to start fresh]

(defun run-population-once (population)  ; [input->output] for each stage is indicated in comments.
  (incf *pop-tick*) ; note that it's incremented before we do anything, and then reported after finishing--but before the next incf
  (update-propn-nets-from-analogy-nets           ; update proposition network links based on activations of proposition-map-units [pop->pop]
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

(defun del-netlogo-outfile ()
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
  (>= *pop-tick* *max-pop-ticks*))

; Variants:

; This variant is useful if you want to be able to run forever:
; It has the result that if *max-pop-ticks* is zero, popco will loop forever.
;(defun time-to-stop ()
;  (and (> *max-pop-ticks* 0)
;       (>= *pop-tick* *max-pop-ticks*)))

; This variant might be useful e.g. if we capture ^C's and make the capture routine setf *stop-run?*:
;(defun time-to-stop ()
;  (or 
;    (>= *pop-tick* *max-pop-ticks*)
;    (user-says-stop)))
;
;(defun user-says-stop () *stop-run?*)


;;-----------------------------------------------------
;; CHOOSE-CONVERSERS 
;; choose-conversers MOVED to social-net.lisp, i.e. replaced by KH's version (2/2013)


;;-----------------------------------------------------
;; CHOOSE-UTTERANCES

;; ARG: A list whose car is a set of pairs of persons (the first of whom will speak to the other)
;;      and the population (not used--simply passed through for the sake of functional-ey-ness).
;; RETURNS: A similar list, but with an interpersonal proposition consed onto each pair of persons.
;;          This will be what the first person says to the second in each case.
;;          Pairs are random without replacement. 
;;          However, if the speaker has nothing suitable to say, the corresponding result
;;          element will be missing--i.e. the result list will be shorter than the argument list.
;;          [If population size is odd, one person will sit out this round.]
;; TODO?: If speaker has nothing to say, then listener looses chance this round to participate in
;;       conversation.  This is maybe undesirable.  e.g. if there are two such speakers, maybe their
;;       listeners ought to get a chance to converse with each other.
(defun choose-utterances (conversers-and-pop)
  (cons 
    (mapcar-when #'choose-utterance (car conversers-and-pop))   ; mapcar-when in popco-utils.lisp
    (cdr conversers-and-pop)))

; CHOOSE-UTTERANCE
; Conses the result of choose-thought onto the speaker/listener pair that's passed as the argument.
; Or if choose-thought can't find any suitable proposition, and returns nil, choose-utterance also returns nil.
(defun choose-utterance (conversepair)
  (let* ((speaker (speaker-of-cpair conversepair))
         (thought (choose-thought speaker)))
    (if thought
      (cons thought conversepair)
      nil)))

; CHOOSE THOUGHT
; Helper function for choose-utterance.
; Currently randomly chooses a proposition from analog structures listed in
; converse-strucs, or from all-propositions if converse-strucs is nil, after
; filtering out any propositions that don't meet a test like exceeding a
; threshold or having a weighted coin flip come up heads.  If no propositions
; pass the test, nil is returned.
; [Note this appends the proposition lists from structures in converse structs before
; making the random choice, rather than randomly choosing a structure first, so
; that all propositions in the specified structures get an equal chance.  So if
; what you want is for all structures to be used, specify converse-strucs as nil;
; this will avoid the expense of the append.]
; TODO?: See commment on choose-utterances.
(defun choose-thought (speaker)
  (let* ((converse-strucs (get speaker 'converse-strucs)) ; list of analog strucs to use as propn sources. nil for all propns.
         (candidate-thoughts 
           (remove-if-not #'seems-worth-saying?
                          (if converse-strucs
                            (apply #'append (mapcar #'propns-from-struc converse-strucs)) ; get propns from designated analog strucs
                            (get speaker 'all-propositions)))))
    (if candidate-thoughts ; <- maybe there was nothing suitable
      (elt candidate-thoughts (random (length candidate-thoughts)))
      nil)))

; SEEMS-WORTH-SAYING?
; Persons are more likely to say things that have a high absolute activation,
; i.e. propns that they believe strongly.  This function determines whether a propn
; Checks whether a random number in [0,1.0) is less than the scaled absolute activation.
; i.e. high value for absolute activation gives more chances for uttering, while
; low values give fewer chances.  Note that if the scaling factor is > 1, then all
; activations above (/ 1 scaling-factor) will have probability of 1.
; If *utterance-probability-increment* is a small number > 0, it will allow
; zero-activation propns to be uttered occasionally.
; See nts/probabilistic-utterance for further discussion.
(defun seems-worth-saying? (propn)
  (< (random 1.0)
     (+ *utterance-probability-increment* 
        (* (abs (activation propn)) 
           *utterance-probability-multiplier*))))

;(defun activn-exceeds-threshold? (propn)
;  (>= (abs (get propn 'activation)) *utterance-threshold*))

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
  ;(when (< *trust* 1.0) (format t " transmitting utterance: ~S ~S~%" conversation (activation (car conversation)))) ; DEBUG [trust test is kludge to exclude env-generated propns]
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
; [NOTE: update-propn-net-from-analogy-net *may* simply reinvoke a semantic-iff that was created here in receive-utterance
; because a new proposition suddenly made it applicable, which might then immediately get clobbered 
; in update-propn-net-from-analogy-net before it could be used.  However, some of the semantic-iffs created here will not correspond
; to the map-unit-influenced ones we deal with there.  So it's simpler to create all semantic-iffs
; relevant to a new proposition here, even if a few of them might get recreated moments later over there.]
(defun receive-utterance (generic-propn generic-struc listener trust)
  (setf *the-person* listener)
  (let* ((personal-propn (generic-to-personal-sym generic-propn))
         (personal-struc (generic-to-personal-sym generic-struc))
         (is-new-thought (if (member personal-propn (get personal-struc 'propositions)) nil t))) ; the IF converts TRUE to T per se
    ;(when (< trust 1.0) (format t " ~S received utterance ~S~%" listener generic-propn)) ; DEBUG [trust test is kludge to exclude env-generated propns]
    ;(setf (get listener 'settled?) nil)  ; obsolete - useful only for comparisons with old code
    (when is-new-thought 
      ;(format t " new thought: ~S added to ~S~%" personal-propn personal-struc) ; DEBUG
      (add-to-struc personal-struc 'start (list (get generic-propn 'message))) ; this does nothing but set fields (also calls note-unit, but that's overrident in next line)
      (init-propn personal-propn *propn-init-activ*)
      (mark-propn-unit-newly-added personal-propn *the-person*)
      (invoke-record-semantic-iffs-for-propn personal-propn *the-person*)
      (SETF (GET LISTENER 'ANALOGY-NET-SETTLED?) NIL)) ; analogy net unsettled only by new thoughts, which add to the net
    (update-salient-link personal-propn (utterance-influence generic-propn trust))
    (SETF (GET LISTENER 'PROPN-NET-SETTLED?) NIL) ; updating salient link unsettles the propn net
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
  ;(format t "update-salient-link for propn ~S with weight ~S~%" propn weight) ; DEBUG
  (when (unlinked? propn 'salient) ; if this link doesn't exist
    (mark-constraint-newly-added propn 'salient weight *the-person*)) ; record that we're making a new constraint, so popco can tell gui if desired
  (raw-make-symlink 'salient propn weight)) ; note this just calls make-link, which merely adds in weight if the link exists

;; INVOKE-SEMANTIC-IFFS-FOR-PROPN
;; NOTE: Causes semantic-iffs to be recorded as newly-added-constraints.
;; NOTE might need to add a max and min weight specification if these are summing with other iff sources such as analogical relationships.
(defun invoke-record-semantic-iffs-for-propn (personal-propn person)
  (setf *the-person* person)
  ;(format t "invoking semantic-iffs ~S~%" (find-semantic-iffs (get person 'semantic-iffs) personal-propn)) ; DEBUG
  (mapc #'apply-record-raw-make-symlink-if-units
        (find-semantic-iffs-by-unit (get person 'semantic-iffs) personal-propn)))  ; note: find-semantic-iffs-by-unit is in popco-utils.lisp

;; INVOKE-SEMANTIC-IFFS-FOR-PROPN-MAP-UNITS
;; NOTE: Does *not* cause semantic-iffs to be recorded as newly-added-constraints.
(defun invoke-semantic-iffs-for-propn-map-units (propn-map-units person)
  (mapc #'apply-raw-make-symlink-if-units
        (find-semantic-iffs-in-unit-pairs (get person 'semantic-iffs)            ; find-semantic-iffs-in-unit-pairs in popco-utils.lisp
                                          (mapcar #'concerns propn-map-units))))

;;-----------------------------------------------------
;; SETTLE-NETS

;; Settle internal network of each person until some perhaps
;; small number of cycles is reached, whether convegred or not.
;; ARGS:    a population
;; RETURNS: the same population, but after each person's network has settled
;;          for *max-times* cycles
(defun settle-nets (population)
  (let ((members (get population 'members)))
    (mapc #'settle-person-propn-net members)
    (mapc #'settle-person-analogy-net members)
    (mapc #'update-settled-properties members)) ; force propn net to be unsettled if analogy net is [do *not* move in between prev lines]
  population)

(defun settle-person-analogy-net (person)
  ;(format t "~%~S settle-person-ANALOGY-net:~%" person) ; DEBUG
  (settle-person-net person 'all-map-units 'analogy-net-settled?)) ; see network.lisp

(defun settle-person-propn-net (person)
  ;(format t "~%~S settle-person-PROPN-net:~%" person) ; DEBUG
  (settle-person-net person 'all-propositions 'propn-net-settled?)) ; see network.lisp

; If analogy net is unsettled, then it could change weights in propn net,
; so force propn net to be unsettled, even if had settled on the previous pop-tick:
(defun update-settled-properties (person)
  ;(format t "update-settled-properties: person = ~S, analogy-net-settled = ~S, propn-net-settled = ~S ... " person (get person 'analogy-net-settled?) (get person 'propn-net-settled?)) ; DEBUG
  (unless (get person 'analogy-net-settled?)
    (setf (get person 'propn-net-settled?) nil)))

;; this might go away in the future if we stop using 'settled?
;(defun update-settled (person)
;  (setf (get person 'settled?)
;        (and (get person 'analogy-net-settled?)
;             (get person 'propn-net-settled?))))

;;;;;;;; FUNCTIONS FOR COMMUNICATION WITH EXTERNAL APPLICATIONS ;;;;;;;;

;; report-conversations and report-persons are the top-level reporting functions,
;; along with report-persons-initially.

;; NOTE formatting functions now moved to popco-fmt.lisp

;; REPORT-CONVERSATIONS
;; Outputs data on conversations for use by an external GUI etc.
;; ARG: conversations/population pair produced by transmit-utterances.
;;      i.e. the conversations have added new-propn flags.
;; RETURNS: the population, unchanged
(defun report-conversations (conversations-plus-and-pop &optional (outstream *netlogo-outstream*))
  (when *do-report-to-netlogo* 
    (if *do-converse*
      (princ (fmt-conversations-for-netlogo (car conversations-plus-and-pop)) outstream)
      (princ "[]" outstream))
    (terpri outstream))
  (cdr conversations-plus-and-pop))

;; REPORT-PERSONS-INITIALLY-FOR-x
;; Currently only do at the beginning, not eg GUESS, which I'm init'ing by hand outside of this code
(defun report-persons-initially-for-netlogo (population &optional (outstream *netlogo-outstream*))
  (princ *netlogo-syntax-description* outstream)
  (report-propn-categories-to-netlogo outstream)
  (report-population-to-netlogo population outstream))

(defun report-persons-initially-for-csv (population)
  (report-pop-propns-csv-header-row population))

(defun report-persons-just-at-t-for-netlogo (population &optional filename)
  ; duplicate functionality of &optional--easier to read here:
  (setf filename (or filename 
                     (format nil "~A~A~A~A" *netlogo-basename* *netlogo-snapshot-suffix* *pop-tick* *netlogo-extension*)))
  (format t "Creating NetLogo snapshot file ~S at tick ~S~%" filename *pop-tick*)
  (with-open-file (outstream filename :direction :output :if-exists :rename :if-does-not-exist :create)
    (report-persons-initially-for-netlogo population outstream)))

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
  (when *do-report-propns-to-csv*
    (report-pop-propns-csv-activns-row population))
  (mark-persons-items-old population) ; since done reporting, no longer need to know what's new, so make it old
  population)

(defun do-final-reports (population)
  (when *do-report-to-netlogo*
    (report-persons-just-at-t-for-netlogo population)))

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

(defun report-propn-categories-to-netlogo (&optional (outstream *netlogo-outstream*))
  (princ (fmt-tree-for-netlogo *propn-category-prefixes*) outstream)
  (princ (fmt-tree-for-netlogo *propn-category-descriptions*) outstream)
  (terpri outstream))

(defun report-status-to-netlogo ()
  (princ (format-netlogo-status-message) *netlogo-outstream*))

(defun report-population-to-netlogo (population &optional (outstream *netlogo-outstream*))
    (princ 
      (fmt-tree-for-netlogo
        (mapcar #'fmt-person-for-netlogo (get population 'members)))
      outstream)
    (terpri outstream))

(defun report-pop-propns-csv-header-row (population)
  (princ (fmt-pop-propn-labels-csv-row population) *propns-csv-outstream*)
  (terpri *propns-csv-outstream*))

(defun report-pop-propns-csv-activns-row (population)
  (princ (fmt-pop-propn-activns-csv-row population) *propns-csv-outstream*)
  (terpri *propns-csv-outstream*))

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
  (merge-groups *all-soc-net-groups*)
  (format t "~%Creating analogy networks for each person ....~%")
  (create-nets population))

(defun create-nets (population)
  (mapc #'create-net (get population 'members))
  (mark-persons-items-old population) ; previous step marked all items new, but we didn't want to know that.
  (record-poss-personal-propns-in-pop population) ; prepare for output to csv file
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
  (update-propn-net-from-analogy-net person) ; initialize proposition net [won't reflect code in next lines, but needed so make-symlinks there have something to attach to]
  (mapc #'apply-record-raw-make-symlink-if-units (get person 'semantic-iffs))
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
  (mapc (lambda (targ)     ; applies constraint-map to all pairs of target, source analogs (usually just one of each)
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
; SEE doc/ruleForUpdatingPropnLinksFromMapNodes for a description of this whole process 5/2103.
(defun update-propn-nets-from-analogy-nets (population)
  (mapc #'update-propn-net-from-analogy-net (get population 'members))
  population)

; Make/update proposition links from proposition-map-units, then reinvoke 
; relevant semantic-iffs, since they get clobbered when we update links from map units.
; [NOTE that when we update a proposition link from a map unit, we *want* to forget the
;  weight that was set from map units on previous iteractions.  It no longer matters.  But we *do*
;  want to remember past semantic-iff's; they should be summed in.  The present strategy is
;  simply to go get the specification of the semantic-iff again and reapply it.]
; [Also NOTE: That *may* simply reinvoke a semantic-iff that was created in receive-utterance
; because a new proposition suddenly made it applicable, but then immediately gets clobbered 
; here before it can be used.  However, some of the semantic-iffs created there will not correspond
; to the map-unit-influenced ones we deal with here--i.e. they might not have been clobbered 
; by the previous step in update-propn-net-from-analogy-net.  So it's simpler to create all semantic-iffs
; relevant to a new proposition there, even if a few of them might get recreated moments later here.]
(defun update-propn-net-from-analogy-net (person)
  (when *do-update-propn-nets*
    (setf *the-person* person) ; [redund if called from create-net]
    (let ((propn-map-units (get *the-person* 'propn-map-units)))
      (mapc #'update-assoc-from-unit propn-map-units) ; from acme-infer.lisp
      (invoke-semantic-iffs-for-propn-map-units propn-map-units person))))

(defun update-propn-nets-from-propn-nets (population)
  (mapc #'update-propn-net-from-propn-net (get population 'members))
  population)

;; TODO
(defun update-propn-net-from-propn-net (person)
  (when *do-update-propn-nets*
    (setf *the-person* person) ; [redund if called from create-net]
    (let ((conditionals (remove-if-not #'conditional-propn-p (get *the-person* 'all-propositions))))
      ;(mapc #'update-assoc-from-unit propn-map-units) ; from acme-infer.lisp
      ;(invoke-semantic-iffs-for-propn-map-units propn-map-units person)
      )))

;; TODO
(defun conditional-propn-p (propn)
  t) ; FIXME

;; TODO
(defun biconditional-propn-p (propn)
  t) ; FIXME

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
; ARG: a generic proposition message, i.e. a full subject/predicate representation in list form.
; NOTE: Typically this has to be called *after* the population has been initialized by init-pop.
; If you get an error indicating that personal-to-generic-sym is trying to convert nil to a personal
; symbol, that might be the problem.
; *THE-PERSON* MUST BE SET CORRECTLY.
; ALSO SEE: perceived-negation
(defun perceived (msg &optional (degree 1.0) (person *the-person*))
  (let* ((env (make-persons-env-sym person))
         (generic-propn (last-element msg))
         (generic-struc (generic-struc-of-propn generic-propn person))
         (env-struc (generic-to-personal-sym generic-struc env)))
    (unless (get person 'env)           ; give the person its environment if doesn't exist
      (setf (get person 'env) env))
    (pushnew env-struc (get env 'all-structures))
    (make-propn env-struc 'ignored msg env)
    (init-propn (generic-to-personal-sym generic-propn env)
                degree env)))

; GENERIC-STRUC-OF-PROPN 
; Roughly: Get the analog struc of a generic propn.  However, generic propns don't have
; analog strucs, because strucs are something contained in persons.  Therefore only
; personal propns have strucs.  So this function,
; given a generic-propn, gets the name of a generic analog struc corresponding 
; to the personal-struc of the corresponding personal-propn for the present person.
; *THE-PERSON* MUST BE SET CORRECTLY unless a person is passed in.
(defun generic-struc-of-propn (generic-propn &optional (person *the-person*))
  (personal-to-generic-sym 
    (personal-struc-of-propn 
      (generic-to-personal-sym generic-propn person))
    person))

; PERSONAL-STRUC-OF-PROPN 
; Get the personal analog structure that a personal proposition belongs to.
; Assumes that the car of the first list in belongs-to is this entity.
(defun personal-struc-of-propn (personal-propn)
  (caar (get personal-propn 'belongs-to)))

; PERCEIVED-NEGATION
; Perceive a propn.  Insert the proposition into a person's "external" 
; environment, setting its activation to -1.
; ARG: a proposition message, i.e. a full subject/predicate representation in list form.
; *THE-PERSON* MUST BE SET CORRECTLY.
(defun perceived-negation (msg &optional (person *the-person*))
  (perceived msg -1 person))

; DROP SALIENCE
; make nothing salient for anyone:
(defun drop-salience ()
  (remove-all-constraints-from 'salient)
  ; THIS SHOULD MAYBE BE DONE WITH CLEAR-PROPN IN acme.lisp:
  (mapc #'(lambda (person) (setf (get (get person 'env) 'all-propositions) '()))
        (get *the-population* 'members)))


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
; Calls to semantic-iff are normally listed as part of the initial-input argument to make-person,
; and are then eval'ed in function create-net, thereby storing info about the semantic iff in the 
; person for later use.
; Grepping won't find a real version of "defun semantic-iff"; the next line is the place to look.
(setf (symbol-function 'semantic-iff) #'generic-semantic-iff)

; PERSONAL-SEMANTIC-IFF
; semantic-iffs will have the structure (propn1 propn2 weight)
; [so that we can just pass the entire list as is to apply].
(defun personal-semantic-iff (personal-propn1 personal-propn2 weight &optional (person *the-person*))
  (pushnew (list personal-propn1 personal-propn2 weight) (get person 'semantic-iffs)))

; ;; OBSOLETE--use semantic-iff:
; ;; MAKE-PERSONAL-SYMLINK-IF-UNITS
; ;; SYMLINK-IF-UNITS
; ;; Convenience function, especially for use in specification of a population.
; ;; Just calls make-symlink-if-units after personalizing generic unit names.
; ;; Allows forcing a link between two nodes within a person, without
; ;; having to embed generic-to-personal-sym's in top-level population code.
; ;; *THE PERSON* MUST BE SET PROPERLY.
; (defun make-personal-symlink-if-units (generic-unit1 generic-unit2 weight)
;   (make-symlink-if-units (generic-to-personal-sym generic-unit1)
;                           (generic-to-personal-sym generic-unit2)
;                           weight))
; 
; ;; OBSOLETE--use semantic-iff:
; ;; Abbreviation for preceding, which see.
; (defun symlink-if-units (generic-unit1 generic-unit2 weight)
;   (make-personal-symlink-if-units generic-unit1 generic-unit2 weight))

; ;; OBSOLETE?--use semantic-iff:
; ;; MAKE-SYMLINK-IF-UNITS
; ;; Make symlink if the two units to be linked are suitable--if they
; ;; have activation values.  Silently returns nil if not.
; ;; Useful for specification of a population model,
; ;; to run the same function on everyone, but don't have it bomb if the
; ;; appropriate beliefs or other nodes don't exist.
; (defun make-symlink-if-units (unit1 unit2 weight)
;   (when (and (unit? unit1)
;              (unit? unit2))
;     (make-symlink unit1 unit2 weight))) ; from network.lisp


; RAW-MAKE-SYMLINK-IF-UNITS
;; Make symlink if the two units to be linked are suitable--if they
;; have activation values.  Silently returns nil if not.
(defun raw-make-symlink-if-units (unit1 unit2 weight)
  (when (and (unit? unit1)
             (unit? unit2))
    (raw-make-symlink unit1 unit2 weight))) ; from network.lisp

; APPLY-RAW-MAKE-SYMLINK-IF-UNITS
(defun apply-raw-make-symlink-if-units (unit1-unit2-weight-list)
  (apply #'raw-make-symlink-if-units unit1-unit2-weight-list))

; APPLY-RECORD-RAW-MAKE-SYMLINK-IF-UNITS
; NOTE: *THE-PERSON* MUST BE SET CORRECTLY.
(defun apply-record-raw-make-symlink-if-units (u1u2w)
  (mark-constraint-newly-added (first u1u2w)  ; unit1
                               (second u1u2w) ; unit2
                               (third u1u2w)  ; weight
                               *the-person*) ; record that we're making a new constraint, so popco can tell gui if desired
  (apply-raw-make-symlink-if-units u1u2w)) 

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
  (setf (get *the-person* 'propn-net-settled?) nil)    ; separate settling? properties for the two person networks
  (setf (get *the-person* 'analogy-net-settled?) nil)
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
