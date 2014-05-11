; FILE: acme-infer.lisp
; PURPOSE: analogical transfer of propositions
; PROGRAMMER: Paul Thagard, adapting Eric Melz's work on Copying with Substitution and Generation
; CREATED: 5/14/98
; UPDATE: 12/1/98. broad-infer fixed to handle emotional analogies.
;
; Many modifications by Marshall Abrams 2011, 2012

; Analogical inference involves transfer of information from a source to a
; target. This can be broad or specific:
; 1. Broad: What does the source tell you about the target?
; - do full CWSG as in Melz, but make transfer more focused
; on the source and target, not on (get *the-person* 'all-propositions).
; 2. More specific: What does the source tell you about particular
; aspects of the target, e.g. predicates and/or objects.
; BROAD-INFER expands the target as fully as possible using all
; source propositions not already mapped to a target proposition.
; SPEC-INFER adds to the target only one proposition.
; The result of both kinds of inference is that a proposition in
; the target is positively linked to the proposition that
; was used to generate it. Analogical inference can be both novel
; and routine. Novel: a new proposition is constructed in the target.
; Routine: an already existing proposition in the target is linked
; to its analog in the source.
; For system mappings, where the propositions are related as part of
; the same causal structure, the links should be much stronger
; than ordinary similarities.
; Forging new activation links should automatically enable transfer
; of valences too, modeling emotional analogies. Inference with
; multiple analogies will also be automatically be modeled, since
; propositions from different source analogs can all be linked to
; a single proposition in the target.

; New variables

(defvar *propn-number* 0)
(defvar *arg-number* 0)
(defvar *min-match-activation* 6/10) ; matches have to be this good
(defvar *ana-assn* 5/10) ; analogical association in absence of mapping hypothesis
(defvar *hypotheses-generated* nil) ; new hypotheses generated


; SPEC-INFER makes possible inference of a single proposition in the target
; supported by its analog in the source. If the proposition to be inferred already
; exists, then all SPEC-INFER does is link it to its analog. Otherwise, it uses
; CWSG to produce a new proposition and link it to its analog.
; Here propn-to-infer is a pair of (propn-from-source propn-from-target)

(defun spec-infer (source target predicate 1-or-more-objects)
  (let ((propn-to-infer (find-propn source target predicate 1-or-more-objects)))
    (unless propn-to-infer ; if it doesn't exist
      (setf propn-to-infer ; make it by cwsg
            (make-propn-to-infer source target predicate 1-or-more-objects)))
    (associate-ana-from-propns (first propn-to-infer) (second propn-to-infer)) ; link the analogs
    (my-print "Making inferential connection: " propn-to-infer)))

; ASSOCIATE-ANA-FROM-PROPNS (formerly associate-ana) associates two analogous 
; propositions, the first from the source, the second from the target. 
; The strength of the association is based on how
; good the mapping is, i.e. on the activation of the unit that represents a
; mapping between the two propositions.  The map unit's name is constructed
; from the names of the two proposition units. -MA 11/2011
; NOTE SINCE THIS IS DESIGNED FOR TRADITIONAL COHERE FUNCTIONALITY
; it doesn't allow passing optional max link weights.
; *THE-PERSON* MUST BE SET PROPERLY.
(defun associate-ana-from-propns (sprop tprop)
  (associate-ana sprop tprop
                     (catname (personal-to-generic-sym sprop)    ; strip person-tags before concatenating
                              (personal-to-generic-sym tprop)))) ; catname will add single person-tag

;; NOT IN USE:
;; ASSOCIATE-ANA-FROM-UNIT is like associate-ana-from-propns, but is given
;; the map unit, and gets the names of the proposition units from the map
;; unit's 'concerns property.  (We can probably assume that the concerns
;; propositions are listed in order source, target, but if not, it won't
;; matter since the link is symmetrical, and we already have the map unit name.) -MA 11/2011
;(defun associate-ana-from-unit (map-unit)
;  (let ((props (get map-unit 'concerns)))
;    (associate-ana (first props) (second props) map-unit)))

; ASSOCIATE-ANA is a basic function creating a symmetric link between two proposition
; units corresponding to a map unit, all of which must be passed as args. -MA 11/2011
; The optional arguments are maximum link weights. -MA 1/2012
; [The functionality of Thagard's associate-ana is now in associate-ana-from-propns.]
(defun associate-ana (prop1 prop2 map-unit &optional (excit-weight *excit-weight*) (inhib-weight *inhib-weight*) (init-activ *init-activ*))
  ;(DECLARE (LONG-FLOAT EXCIT-WEIGHT INHIB-WEIGHT INIT-ACTIV))
  (associate prop1 prop2                                ; link the two proposition units [associate from imp.lisp]
             (or (get map-unit 'activation) *ana-assn*) ; initial weight of the link
             (or (activation prop1) init-activ)       ; initial activation of source propn, or init-activ if nil
             (or (activation prop2) init-activ)
	     excit-weight
	     inhib-weight))                       ; initial activation of target propn

; OLD VERSION OF ASSOCIATE-ANA (now called associate-ana-from-propns above)
; [a modified version of Thagard's original function]
;(defun associate-ana (sprop tprop)
;  (associate sprop tprop
;             (or (get (catname (personal-to-generic-sym sprop)  ; strip person-tags before concatenating
;                               (personal-to-generic-sym tprop)) ; catname will add single person-tag [p-t-g-s added 10/2011 -MA]
;                      'activation) 
;                 *ana-assn*)
;             (activation sprop)    ; nil if no activation--
;             (activation tprop)))  ;  then associate will use default  [these lines added by MA 10/2011]


; SET-ASSOC-WEIGHT-FROM-ACTIVN: Set the weight of an existing association
; between propositions from the activation of an ACME map unit.  -MA 11/2011
(defun set-assoc-weight-from-activn (prop1 prop2 map-unit &optional (excit-weight *excit-weight*) (inhib-weight *inhib-weight*))
  ;(DECLARE (LONG-FLOAT EXCIT-WEIGHT INHIB-WEIGHT))
  (set-assoc-weight prop1 prop2 (activation map-unit) excit-weight inhib-weight)) ; set-assoc-weight is in imp.lisp

;; NOT IN USE:
;; SET-ASSOC-WEIGHT-FROM-UNIT: Set the weight of an existing association
;; between propositions from the activation of an ACME map unit, first
;; figuring out what the propositions corresponding to the map unit are.  -MA 11/2011
;(defun set-assoc-weight-from-unit (map-unit)
;  (let ((props (get map-unit 'concerns)))
;    (set-assoc-weight-from-activn (first props) (second props) map-unit)))

; UPDATE-ASSOC: Create association if it doesn't exist, 
; or set link weight from map-unit's activation if not. -MA 11/2011
; NOTE: Since this is designed for setting proposition-inference network link weights
; in POPCO, we pass alternative max link weights to associate-ana. -MA 1/2012
(defun update-assoc (prop1 prop2 map-unit)
  (if (symlinked? prop1 prop2)
    (set-assoc-weight-from-activn prop1 prop2 map-unit 
                                  *propn-excit-weight* *propn-inhib-weight*)
    (associate-ana prop1 prop2 map-unit 
                   *propn-excit-weight* *propn-inhib-weight* *propn-init-activ*)))

; NOTE ON UPDATE-ASSOC ABOVE:
; CONSIDER FOR FUTURE MODIFICATION OF UPDATE-ASSOC (SEE PROPN-LINKS.NTS):
; Consider replacing the references to *propn-*-weight* with calls to functions.
; Rather than setting the difference between propn-excit/inhib-weight globally, 
; we could use these to make these scaling factors differ for each subnet, depending on
; how many links each node is being supplied by the analogy network.  
; e.g. if there are three links per propn node, the two negative links could be scaled
; by half of the scaling factor for the positive node, but if there are five links
; per node, the four negative links could be scaled by 1/4 of the excitatory
; scaling factor.  That way, the overall scaling of the negation of the excitatory
; relation is not given extra weight when there are many map competitors.
; ** WARNING: DO NOT DO THIS WITH MACROS THAT REFERENCE GLOBALS SUCH AS *propn-inhib-weight*,
;    or use care if you do.
;   Some implementations, such as sbcl, will grab the global value and compile it into the
;   macro as soon as the macro definition is encountered.  If you change the variable later,
;   e.g. in the code for your specific model, the new value would then be ignored.

; UPDATE-ASSOC-FROM-UNIT: Extract concerns propositions from map unit, call update-assoc -MA 11/2011
; NOTE: See comment on update-assoc about max link weights. 
(defun update-assoc-from-unit (map-unit)
  (let ((props (get map-unit 'concerns)))
    (update-assoc (first props) (second props) map-unit)))

; FIND-PROPN looks in a target structure to see if there is a proposition involving
; a given predicate and 1 or more objects. If so, it returns a list
; of the name of the analogous proposition in
; the source, if it exists and the proposition name. Otherwise, it returns nil.
(defun find-propn (source target predicate 1-or-more-objects)
  (do ((propns (get target 'propositions) (cdr propns)))
      ((null propns) nil) ; return
    ;repeat
    (if (propn-matches (list predicate 1-or-more-objects)
                       (get (car propns) 'message))
        (return (list (or (best-match-in-struc source (car propns)) ; return exits the do-loop
                          (find-propn-unmapped source (car propns)))
                      (car propns))))))

; FIND-PROPN-UNMAPPED handles the case where there is no best-match-in-struc
; because the proposition is newly created by another analogy.
; NOTE: this may not be the best match, but it would rarely matter.
(defun find-propn-unmapped (source propn)
  (do ((propns (get source 'propositions) (cdr propns)))
      ((null propns) nil)
    ; repeat
    (if (propn-matches (get propn 'message)
                       (get (car propns) 'message))
        (return (car propns))))) ; found

; PROPN-MATCHES determines if two messages have a similar
; predicate and at least some of the same arguments.
(defun propn-matches (mess1 mess2)
  (and (or (equal (car mess1) (car mess2))     ; are predicates identical?
           (similar? (car mess1) (car mess2))) ; or marked semantically similar?
       (intersection (second mess1) (second mess2)))) ; and are some objs common?

; BEST-MATCH-IN-STRUC is like BEST-MATCH in acme.lisp, except that it
; should make sure, for the sake of multiple analogies, that the match found
; is from the specified structure. This works with predicates, objects,
; and propositions. Using the constructed-matches property, it
; checks for matches that have been constructed by generation.
(defun best-match-in-struc (struc el)
  (let (bm bm2 best-unit other-good-units)
    (setf best-unit (highest-l (constraints-for-structure struc el)
                               'activation))
    (cond ((null best-unit) nil) ; nothing matches
          ((match-generated struc el) (match-generated struc el))
          (t
           ; else do all of these:
           ; ADD remove matches from other structures - NOT YET implemented
           (if (listp best-unit)
               (setf best-unit (car (setf other-good-units best-unit))))
           (setf bm (other-from-pair el (get best-unit 'concerns)))
           (my-print "Best mapping of " el " in " struc " is " bm ". "
                     (get best-unit 'activation))
           (if other-good-units
               (do ((units (cdr other-good-units) (cdr units)))
                   ((null units) nil)
                 (setf bm (other-from-pair el (get (car units) 'concerns)))
                 (my-print " tied with " bm2 ".")))
           (if (> (get best-unit 'activation) *min-match-activation*)
               bm ; return best match if it has high enough activation
             ; otherwise return nil
             nil)) ; end t
          ) ; end cond
    ) ; end let
  )

; MATCH-GENERATED looks for a match that has been previously constructed
; by make-propn-to-infer using make-arg

(defun match-generated (struc ele)
  (cdr (assoc ele (get struc 'generated-matches))))

; CONSTRAINTS-FOR-STRUCTURE (struc el) returns a list of all the constraint-hyps
; for an element - object, predicate, or proposition -
; that are relevant to the given structure. This is for multiple
; analogies, where different mappings can co-exist.
(defun constraints-for-structure (struc el)
  (do ((constraints (get el 'constraint-hyps) (cdr constraints))
       (result nil))
      ((null constraints) result)
    (if (constraint-relevant struc (car constraints))
        (push (car constraints) result))))

; CONSTRAINT RELEVANT returns T if the given constraint appertains to
; a given source structure.
(defun constraint-relevant (struc constraint)
  (member (car (get constraint 'concerns))
          (elements-from-structure struc)))

; ELEMENTS-FROM-STRUCTURE returns a list of all the predicates, arguments, and
; propositions from a structure.
(defun elements-from-structure (struc)
  (do ((propns (get struc 'propositions) (cdr propns))
       (result nil))
      ((null propns) result)
    (setf result
          (union-list (list (car propns)
                            (pred-from-propn (car propns)))
                      (args-from-propn (car propns))
                      result))))



; MAKE-PROPN-TO-INFER uses information in the source to produce a new proposition
; in the target to infer. The predicate in the new proposition is the
; same as in the source, but the arguments are completed by either
; subsitution (using the best match) or generation of a new argument.
; The predicate must have a corresponding predicate in the source.
; Should there be a similar check for objects?
; Returns a pair of (propn-from-source propn-from-target)

(defun make-propn-to-infer (source target pred object-lst)
  (let ((new-message nil)
        (new-propn (gen-target-propn target pred))  ; this just creates the proposition symbol, i.e. generates the name
        (source-propn (find-source-propn source pred)))
    (if (null source-propn) ; no source proposition found
      (progn (my-print "No source analog found for " pred " and " object-lst)
             nil) ; return nothing
      ; else construct the message
      (do ((arguments (reverse (new-args-from-propn source-propn)) (cdr arguments))
           (new-arguments nil))
          ((null arguments) ; exit: construct new message
           (setf new-message (list pred new-arguments new-propn))
           (setf (get new-propn 'message) new-message)
           (push new-message (get target 'all)) ; ADD more specific field
           (push new-propn (get target 'propositions))
           (unless (get new-propn 'belongs-to)             ; added by MA 10/15/2011 for field that's present in original propositions but not generated ones
             (setf (get new-propn 'belongs-to) '(nil) ))   ; initialize field if necessary
           (push target (car (get new-propn 'belongs-to))) ; record structure of new proposition
           ; store match with source:
           (setf (get source 'generated-matches)
                 (acons source-propn new-propn (get source 'generated-matches)))
           (process-cause new-propn) ; provide input to ECHO if causal
           (list source-propn new-propn)) ; return pair
        ; repeat construction of arguments--i.e. make names of arguments based on corresponding source objects
        (setf new-arguments (push (make-arg source (car arguments)) new-arguments))))))

; NEW-ARGS-FROM-PROPN is slightly different from the original in ACME which
; has an additional screening function.

(defun new-args-from-propn (propn)
  (second (get propn 'message)))

; FIND-SOURCE-PROPN makes sure that there is a source proposition that can
; provide information about the target predicate. The source proposition has
; to have the same predicate as the predicate to be inferred about.
; This takes the first occurrence - ideally it should make more than
; one possible inference.

(defun find-source-propn (source pred)
  (do ((propns (get source 'propositions) (cdr propns)))
      ((null propns) nil); exit - nothing found
    ; repeat
    (if (equal pred (car (get (car propns) 'message)))
        (return (car propns)))))

; MAKE-ARG takes the analogous object if there is one, and generates
; a new one ARG-NEW-# if there isn't.
; If the argument is a list, as in (CAUSE (CAUSE1 CAUSE2 ...) EFFECT),
; need to iterate.

(defun make-arg (source arg)
  (let ((new-arg nil))
    (if (listp arg) ; argument is a list
      (make-all-args source arg)
      ; else argument is is an object
      (or (best-match-in-struc source arg)
          (match-generated source arg)
          (progn (incf *arg-number*)
                 (setf new-arg (concat-2 (concat-2 arg '-new-) *arg-number*))
                 (setf (get source 'generated-matches)
                       (acons arg new-arg (get source 'generated-matches)))
                 new-arg)))))

; MAKE-ALL-ARGS makes an argument consisting of a list of objects.

(defun make-all-args (source arg)
  (do ((args (reverse arg) (cdr args))
       (result nil))
      ; exit
    ((null args) result)
    ; repeat:
    (setf result (push (make-arg source (car args)) result))))


; GEN-TARGET-PROPN generates a new proposition to be part of a target

(defun gen-target-propn (target pred)
  (let ((new nil))
    (setf new (concat-2 (concat-2 pred '-new- ) *propn-number*))
    (push new (get target 'propositions))
    (proposition new "Hypothesis")
    (incf *propn-number*)
    (my-print "New proposition " new " made for " target)
    (push new *hypotheses-generated*)
    new)) ; return name of proposition

; CONCAT-2 makes a new atom out of two given atoms; cf. CATNAME in acme.lisp
; CONCAT in acme.lisp combines any number of atoms.
; TODO? Should this be replaced by the CL primitive CONCATENATE? -MA
(defun concat-2 (atm1 atm2)
  (read-from-string (coerce (append (coerce (princ-to-string atm1) 'list)
                                    (coerce (princ-to-string atm2) 'list))
                            'string)))

; PROCESS-CAUSE takes a newly generated proposition and produces input
; to ECHO if appropriate. To be added: other predicates for ECHO and DECO.

(defun process-cause (propn)
  (cond ((equal (pred-from-propn propn) 'cause)
         (explain (first (new-args-from-propn propn))
                  (second (new-args-from-propn propn))))))



; MY-SORT alphabetizes
(defun my-sort (lst) (sort lst #'string-lessp))

; BROAD-INFER does full pattern completion on a source, generating
; new propositions for every source proposition that lacks an analogous
; target proposition. This function is analogous to Melz's TRANSFER.
; But it also creates links to make inferences. For each
; proposition of the source, it either finds the most analogous
; proposition of the target, or else creates a new one.
; BUGS 12-1-98: slight redundancy in making of propositions and associations.
; 3-19-99: need to reverse proposition list to ensure causes get
; processed last.

(defun broad-infer (source target)
  (do ((propns (reverse (get source 'propositions)) (cdr propns))
       (message nil)
       (ana-prop nil) ; analogous proposition
       (pairs-to-connect nil))
      ((null propns)
       (my-print source " applied to " target " yields: " )
       (pl target)
       (my-print "Making inferential connections: " pairs-to-connect)
       (associate-pairs pairs-to-connect)) ; link the analogs using associate-ana
    ; repeat
    (setf message (get (car propns) 'message))
    (my-print "Transferring " message)
    (setf ana-prop (best-match-in-struc source (car propns)))
    (if ana-prop ; a good match already exists
      (push (list ana-prop (car propns)) pairs-to-connect)
      ; else make a new proposition to infer
      (push (make-propn-to-infer source target (car message) (second message))
            pairs-to-connect))))

; ASSOCIATE-PAIRS takes a list of pairs and establishes a positive constraint
; between each pair.
(defun associate-pairs (lst)
  (do ((pairs lst (cdr pairs)))
      ((null pairs) (my-print "Inferences made."))
    (associate-ana-from-propns (caar pairs) (second (car pairs)))))


; ACTIVATE-PROPOSITIONS initializes all propositions for further inference.

(defun activate-propositions ()
  (mapcar #'activate-prop (get *the-person* 'all-propositions)))

; ACTIVATE-PROP

(defun activate-prop (prop)
  (put prop 'activation *init-activ*)
  (put prop 'valence *init-activ*))


; FUNCTIONS ADDED BY MARSHALL ABRAMS 2011:

(defun has-credence (propn)
  (includes-credence (get propn 'message)))

; a proposition message includes a credence if it's (at least) length 4
; and its third (its truth-value position) is formally like a credence.
(defun includes-credence (msg)
  (and (>= (length msg) 4)
       (is-credence (get-tr-val msg))))

; A value is a credence if it's a number between -1.0 and 1.0.
; [This is an extension of the usual Bayesian credence range of 0 to 1,
; allowing degrees of disbelief, i.e. degree of belief in the negation.]
(defun is-credence (cred)
  (and (numberp cred)
       (<= cred 1)
       (>= cred -1)))

