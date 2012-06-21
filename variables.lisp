; FILE: variables.lisp
; PURPOSE: global variable initializations for networks
; PROGRAMMER: Paul Thagard, Greg Nelson
; CREATED: 12-9-87
; UPDATED: 5-13-88
; 10-20-97 PT
;
; THIS VERSION MODIFIED 2011, 2012 BY MARSHALL ABRAMS

; does not need compiling.



; ******************************************************
; Global variables:
; ******************************************************

; FOR COHERE

(defvar *problem* nil)
(defvar *show-solutions* 25)
; (setf (get *the-person* 'all-constraints) nil) ; now in initialize-person-properties in popco.lisp
(defvar *weight-of-all-constraints* 0)
(defvar *connect-solution* nil)
(defvar *count-solution* nil)
(defvar *point-5-solution* nil)
(defvar *greedy-solution* nil)
(defvar *greedy-flips* 30 "number of flips in greedy algorithm")
; (setf (get *the-person* 'number-units) nil) ;"association list of numbers and unit names") ; now in initialize-person-properties in popco.lisp
; (setf (get *the-person* 'total-units)  0); "length of (get *the-person* 'all-units)") ; now in initialize-person-properties in popco.lisp
(defvar *eval-mode* 'tempered) ; coherence mode: pure, tempered, or foundational
(defvar *num-solutions* 0) ; number of solutions to be considered
(defvar *max-num-solutions* 5000) ; maximum number of solutions to try
(defvar *special-register* 1L0) ; hold value for *special-activation* DEFINED BELOW [cf. functions pure, temp, found and following comments in cohere.lisp]
(defvar *resonance-impact* 1L0) ; resonance has effect if value > 1

 ; If both of these variables are non-nil, then run-hyp-net
; will report activations of listed nodes for listed persons:
(defvar *watched-nodes* nil)
(defvar *watched-persons* nil)


; PARAMETERS FOR NETWORK OPERATION
(defvar *report-activn-change* nil) ; If non-nil run-hyp-net calculates, returns total change of all activations of person's units. added by MA 1/2012
(defvar *all-xs* nil)    ; This is setf'ed in network.lisp.  Added by MA 10/2011 to make SBCL compiler happy.
(defvar *all-xsit* nil)  ;  ditto
(defvar *init-activ* .01L0 "Initial activation of units.")
(defvar *propn-init-activ* .0L0 "Initial activation of units for POPCO proposition-inference networks.")
(defvar *default-activ* .01L0 "Default activation of units.")
(defvar *excit-weight* .04L0 "Weight of excitatory links--for traditional COHERE networks.")
(defvar *inhib-weight* -.06L0 "Weight of inhibitory links--for traditional COHERE networks.")
(defvar *propn-excit-weight* .2L0 "Weight of excitatory links for POPCO proposition-inference networks.")
(defvar *propn-inhib-weight* -.025L0 "Weight of inhibitory links for POPCO proposition-inference networks.") ; NOTE: This may need to be adjusted wrt proportion of neg to pos links
(defvar *simpl-impact* 1L0 "Impact of complexity of structure on coherence.")
(defvar *goal-excit* .04L0 "Default weight of links from special unit to basic goals.")
(defvar *weak-incohere-impact* .2L0 "Impact of incohering subgoals.")
;defined below: (defvar *analogy-impact* 1L0 "Impact of analogous goal structures.")
(defvar *special-activation* 1.0L0 "Activation of special unit.") ; Changed from 1 to 1.0 for implementations like SBCL that are finicky about numeric types -MA 8/3/2011
(setf (get 'special 'activation) *special-activation*)
;(setf (get 'semantic 'activation) *special-activation*)
(defvar *min-activation* -.99L0 "Minimum possible activation for a unit.")
(defvar *max-activation* .99L0 "Maximum possible activation for a unit.")
(defvar *goal-min-activation* 0L0 "minimum possible activation for a goal.")
(defvar *decay-amount* 0.1L0 "Amount that units' activations decay over time.")
(defvar *output-threshold* 0L0 "Minimum activation for an influential unit.") ; PT 2-93
(defvar *min-settle* 25 "Minimum time at which a network can settle.")
(defvar *asymptote* .0001L0 "Amount of change at which a unit has asymptoted.")
(defvar *current-excit* 0.0L0 "Variable for Grossberg rule.")
(defvar *current-inhib* 0.0L0 "Variable for Grossberg rule.")
(defvar *where-to-print* *standard-output* "Option to change output stream.")
(defvar *when-to-print* '(1 30 40 50 70 85 100 150) "When to print state of network.") ; PT 2-93
(defvar *silent-run?* nil "Printout results.")
(defvar *stop-settled?* t "Stop if the network has settled.")
; (setf (get *the-person* 'settled?) nil); "Network has settled.") ; now in initialize-person-properties in popco.lisp
(defvar *stop-run?* nil "Use grossberg's rule for network.")  ; text has to be wrong -M.A.  2011
(defvar *tversky?* nil "Use altered form when activation is negative.")
(defvar *grossberg?* nil "Alternative activation function.")
(defvar *testnum* 1)
(defvar *verbose* t)
(defvar *max-times* 300 "maximum number of cycles of updating")
(defvar *acme-mode* nil )

; FOR GRAPHICS ;

;(defvar *use-actgraph* t "Use the activation grapher.")
(defvar *actgraph-window* nil "Name of activation grapher window.")
(defvar *netgraph-window* nil "Name of network grapher window.")
(defvar *grapher-font* nil "Name of the grapher font.")

(defvar *debug* nil "Turn on the debugger.")
(defvar *devowel-length* 30 "Length beyond which vowels will be taken out.")
(defvar *time-step* 0 "Added by Paul.")



(defvar *netwin-height* 6 "Height in inches of grapher window.")
(defvar *netwin-width* 6 "Width in inches of grapher window.")

;(defvar *netwin-x* (* *netwin-width* *pixels-per-inch-x*) "Window width.")
;(defvar *netwin-y* (* *netwin-height* *pixels-per-inch-y*) "Window height.")
(defvar *netwin-x* nil "Window width.")  ; M.Abrams
(defvar *netwin-y* nil "Window height.") ; M.Abrams
(defvar *netwin-xs* nil "Window starting x position.")
(defvar *netwin-ys* nil "Window starting y position.")
(defvar *unit-location-alist* nil "A-list for relating locations with units, for mouse-selection.")
(defvar *unit-radius* 5 "Default size for unit circles.")
(defvar *nethoriz-spc* nil "X distance between adjacent units.") ; different than spc's for act-grapher
(defvar *netvert-spc* nil "Y distance between adjacent units.")
(defvar *drawing-mode* 'circle "Determines whether to draw circle or squares.")
(defvar *x-radius* nil "X radius of the circle on which units are plotted.")
(defvar *y-radius* nil "Y radius of the circle on which units are plotted.")
(defvar *second-label* 'actstring "Put activation as a second label at each node.")
(defvar *link-labels* 'weight-link-label
  "The function to print as label on links, when non-nil.")
(defvar *link-width* 'weight-link-width
  "The function which determines widths, when non-nil.")
(defvar *known-functions* nil
  "Variable listing which functions can be graphed.")

(defvar *run-type* nil "If value is 'slow, events will be handled during run.")
(defvar *last-init* nil "Name of grapher last initialized.")
(defvar *trace-list* nil "List of units to trace.")

(defvar *use-actgraph* nil "Activation grapher is on or off.") ; these replace
(defvar *use-netgraph* nil "Network grapher is on or off.") ; *last-init*



; FOR ACME:

; (setf (get *the-person* 'all-structures) nil) ; now in initialize-person-properties in popco.lisp
(defconstant +deep-isomorphic-matching+ t) ; MA 4/1/2012
(defvar *struc1* nil)
(defvar *start-units* nil)
(defvar *goal-units* nil)
(defvar *effector-units* nil)
;(defvar *object-units* nil) ; not used in POPCO; replaced with a local var in make-hyp-unit -MA 4/2012
(defvar *sim-list* nil)
(defvar *no-sim-weight* 0L0)
(defvar *obj-conc-fraction* 1L0 ) ; inhibition of object hypotheses
(defvar *stop-many-one* 1L0) ; if > 1L0, discourages many-one mappings
(defvar *propn-uniqueness* 1L0) ; enforces 1-1 mappings of propositions
(defvar *map-all?* nil) ; map regardless of fields

(defvar *ident-weight* .1L0) ; similarity of concept to self
(defvar *synon-weight* .08L0) ; similarity of synonyms
(defvar *coord-weight* .06L0) ; similarity of coordinates
(defvar *synonyms* nil)
(defvar *same-kinds* nil)
(defvar *same-parts* nil)

(defvar *map-one-one?* t) ; do 1-1 mapping
(defvar *no-concept-weight* 0L0) ; weight from special to null units
(defvar *stop-when-matched?* nil) ; for automatic stopping on correct answer
(defvar *best-matches* nil)
(defvar *desired-matches* nil)
(defvar *use-nothing-maps?* nil "If t, construct no-concept maps.")



(defvar *look-for-queries?* nil) ; for query arguments
(defvar *query-connections* nil)
(defvar *query-weight-proportion* 1L0)
(defvar *link-concepts-objects?* t) ; link concept hyps to object hyps directly
(defvar *link-objects?* t) ; make links between object mappings
(defvar *watch-for-dup-arguments?* nil) ; see dup-arguments
(defvar *pragmatic-unit-made* nil) ; for pragmatic constraint

(defvar *show-others?* t) ; show other good maps
(defvar *min-good* .2L0) ; good enough to notice
(defvar *prag-weight* .3L0) ; weight to pragmatic unit for presumed
(defvar *import-weight* .1L0) ; importance links to pragmatic unit

(defvar *use-arcs-semantics?* nil) ; to use arcs semantics file to make
; similarity judgements
(defvar *feature-selection* nil)
(defvar *ignore-preds* nil)
(defvar *propns-import?* 0L0)
(defvar *selection-list* nil) ; for ARCS
(defvar *symmetric-concepts* nil)
(defvar *use-auto-prag?* nil) ; to have function check-importance invoked
(defvar *use-selection-list?* nil) ; for ARCS
; (setf (get *the-person* 'all-concepts) nil) ; now in initialize-person-properties in popco.lisp
; (setf (get *the-person* 'all-objects) nil) ; now in initialize-person-properties in popco.lisp
; (setf (get *the-person* 'all-preds) nil) ; now in initialize-person-properties in popco.lisp



; FOR ECHO:

(defvar *echo2-mode* T "T if running ECHO2.")
;defined below: (defvar *experiment* nil)
(defvar *all-explainers* nil)
(defvar *all-explained* nil)
(defvar *explan-data* nil)
(defvar *data-excit* .05L0) ; excitation of data
;defined above: (defvar *special-activation* 1L0)
; (setf (get *the-person* 'all-propositions) nil) ; now in initialize-person-properties in popco.lisp
(defvar *all-data* nil)
(defvar *data-self-links?* nil)
(defvar *analogy-impact* 1L0) ; impact of analogy
;defined above: (defvar *simpl-impact* 1L0) ; impact of simplicity
(defvar *co-hyp-importance* 1L0) ; if < 1, links between cohypotheses are less.
(defvar *self-link-data* nil)
(defvar *data-init-activ* .01L0)
;defined above: (defvar *init-activ* .01)
(defvar *decay-register* .05L0)
(defvar *check-unexplained* nil) ; look for unexplained data
(defvar *entail-impact* 1L0) ; if > 1, entailment excites more than expln.
(defvar *contradictions* nil) ; pairs of contradictory propns
(defvar *ad-hoc-factor* 1L0 "decrease excitatory links for ad hoc explanations")



; FOR DECO

; (setf (get *the-person* 'all-propositions) nil); "List of all propns.") ; now in initialize-person-properties in popco.lisp
; (setf (get *the-person* 'all-units) nil); "List of all units.") ; now in initialize-person-properties in popco.lisp
(defvar *all-supergoals* nil "List of all supergoals.")
(defvar *all-subgoals* nil "List of all subgoals.")
(defvar *all-basic-goals* nil "List of all basic goals.")
;defined above: (defvar *contradictions* nil "List of lists of contradictory goals.")
; (setf (get *the-person* 'asymptoted-units) nil); "Units that have reached asymptote.") ; now in initialize-person-properties in popco.lisp
; (setf (get *the-person* 'total-links) 0L0); "Total number of links created.") ; now in initialize-person-properties in popco.lisp
; (setf (get *the-person* 'total-times) 0); "Number of settle cycles that have been run.") ; now in initialize-person-properties in popco.lisp
(defvar *experiment* nil "A description of the current experiment.")

; FOR HOTCO

(defvar *emote* nil "Do updating for HOTCO.")
; (setf (get *the-person* 'all-valence-units) nil); "List of units that have valences.") ; now in initialize-person-properties in popco.lisp
(defvar *valence-weight* .05L0 "Default value for valence weight.")
(defvar *truth-valence* 0L0 "Value of truth.")
;; THIS HAS TO BE FIXED:
(setf (get 'special 'valence) 0L0) ; extent to which truth is desired??? ; now in initialize-person-properties in popco.lisp
; not quite the right interpretation, because true theories are
; wanted, but not ugly truths about someone being evaluated.
; (setf (get *the-person* 'evaluation-units) nil) ; for HOTCO 2 ; now in initialize-person-properties in popco.lisp


; POPCO-SPECIFIC VARIABLES:

(defvar *use-new-random-state* t) ; set to nil to use whatever seed the Lisp implementation provides by default

(defvar *perceived-excit* .5) ; default link weight to salient for propositions perceived as true in env
(defvar *special-units* '(special salient pragmatic)) ; units which require special handling by personalization code

(defconstant +acme-max-weight+ .5) ; Used in make-symlink to tamp down on cyclic non-settling in analogy networks.  A bit of a kludge--should be reworked if POPCO starts using ECHO, for example.

(defvar *the-person* 'the-person) ; value normally swapped among members of population. Setting to 'the-person also makes it work with traditional COHERE.
(defvar *the-population* 'the-population)
;(defvar *dont-converse* nil) ; OBSOLETE
(defvar *do-converse* t)
;	Set previous to nil to stop conversations between persons:
;	Causes choose-conversers to return empty list of conversers
;	 so subsequent conversation functions have nothing to do.
;(defvar	*dont-update-propn-nets* nil) ; OBSOLETE
(defvar	*do-update-propn-nets* t)
;	Set previous to nil to cause proposition-net to be neither created 
;	nor updated. Propositions exist regardless, but won't have activations
;	 or links.  Causes update-proposition-nets to be a no-op.
(defvar *do-report-to-netlogo* t)
(defvar *do-report-propns-to-csv* nil) ; whether to write proposition activations to a csv file
(defvar *do-report-analogy-nets-to-guess* t)
(defvar *netlogo-status-flag* "status:")
(defvar *netlogo-status-message* nil)
(defvar *utterance-threshold* .2 "If in use, propns with activns below this number will not be uttered.")
(defvar *utterance-probability-multiplier* 1 "Parameter for utterance-probability-cutoff.")
(defvar *utterance-probability-increment* 0 "Parameter for utterance-probability-cutoff.")
(defvar *trust* .05 "Degree to which utterances are like observations for listeners.")
(defvar *sleep-delay* nil) ; if non-nil, gives number of seconds to pause between pop-ticks
(defvar *max-pop-ticks* 50) ; max number of pop-tick iterations; 0 for infinite
(defvar *pop-tick* 0)
(defvar *persons-reporting-to-guess* '()) ; persons whose data we want to send to GUESS
(defvar *write-person-graphs-at-pop-ticks* '()) ; if pop-tick equals this, then call write-person-graphs using a tick-named directory under *person-graph-basename*.
(defvar *person-graphs-basename* ".")
(defvar *graphml-edge-multiplier* 10)
(defvar *graphml-node-multiplier* 10)
(defvar *graphml-node-floor* 3) ; size of zero-activation node
; Parameters for logistic.lisp; should be tuned for normalize-degree in popco.lisp:
(defconstant +logistic-growth-rate+ 35)  ; sometimes called B
(defconstant +logistic-position+ .3) ; sometimes called M
(defconstant +logistic-taughtness+ 1) ; sometimes called v

(defvar *propn-category-prefixes* '()) ; used to tell e.g. NetLogo prefixes of propositions which identify distinct categories to be measured
(defvar *propn-category-descriptions* '()) ; similar


(proclaim '(type (float)
            min-activation max-activation output-threshold
            excit-weight inhib-weight decay-amount
            current-excit current-inhib))
