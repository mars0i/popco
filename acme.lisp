; FILE: acme.lisp
; PURPOSE: analogical constraint mapping
; PROGRAMMER: Paul Thagard
; CREATED: 5-18-87
; UPDATED: 6-12-88
; UPDATED: 6-21-88 - objects now taken care of in make-obj-unit.
; make-obj-units rewritten as a do loop.
; UPDATED: 7-23-88 - Improvements to query mechanism
; UPDATED: 8-12-88 - check-importance added and made option for constraint
; map
; UPDATED: 5-95, to join COHERE
; UPDATED: 11-98, fixed sem-similarity

; MODIFIED in many ways by Marshall Abrams 6/2011 and after.


; MACRO ISOMORPHIC-ARGLISTS
; This is used in make-hyp-unit and possibly make-obj-unit to determine how 
; deeply proposition argument lists are checked for compatibility.  
; i.e. the value of the constant +deep-isomorphic-matching+ determines 
; (probably at compile time) which of two functions is called at run time.
; Replaces PT's earlier arg length check and call to type-compatible in
; make-hyp-unit, which shallow-isomorphic-arglists now implements.
; See notes at the two function definitions for details about their differences.
(defmacro isomorphic-arglists (args1 args2)
  (if +deep-isomorphic-matching+
    `(deep-isomorphic-arglists ,args1 ,args2)      ; new (2012) recursive arglist checking of propn arglist type compatibility
    `(shallow-isomorphic-arglists ,args1 ,args2))) ; older COHERE-style checking of propn arglist type compatibility


; ******************************
; DATA STRUCTURES AND ABSTRACTION
; *******************************

; MAKE-STRUC creates a structure with a name, type, and an unlimited
; number of fields. E.g. a problem will be:
; Name of structure:
; structure type
; start conditions
; goals
; constraints (optional)
; Each of some-fields has the structure:
; (property (list of propositions))

; Modification to make-struc (GHN, 6/23/88):
; If the flag use-selection-list? (nil by default) is non-nil, it will not
; make the structure unless it is listed in the global variable selection-list.
; This allows ARCS routines to select certain structures from the entire data-
; base which will be used on a given run.

; *THE-PERSON* MUST BE SET PROPERLY
(defun make-struc (name type &rest some-fields)
  (when (and *use-selection-list?* (not (member name *selection-list*))) ; "if" to "when" MA 3/19/2012
    (return-from make-struc (unless *silent-run?* (my-print "Structure ignored: " name))))
  (let ((personal-name (generic-to-personal-sym name)))
    (setf (get *the-person* 'all-structures)
          (cons-if-new personal-name (get *the-person* 'all-structures))) ; changed cons to cons-if-new -MA 6/2011
    (put personal-name 'data-type type)
    (do ((fields some-fields (cdr fields)))
      ((null fields) (unless *silent-run?* (my-print '"Structure made: " personal-name)))
      (put personal-name 'fields (cons (caar fields) (get personal-name 'fields)))
      (put personal-name (caar fields) (second (car fields)))
      ; set up propositions:
      (make-propns personal-name             ; structure name (a symbol)
                   (caar fields)             ; field name (e.g. 'start) ; NOTE: IRRELEVANT IN POPCO (?)
                   (second (car fields))))   ; list of propositions in message format
    personal-name))

; ******************************
; ADD-STRUC adds to an existing structure:

; FOR POPCO USE, SHOULD ONLY BE PASSED PERSONAL ANALOG STRUCTRES
; AS FIRST ARG.  THEREFORE ...
; *THE-PERSON* MUST BE SET PROPERLY (?)
(defun add-to-struc (struc field msgs)
  (put struc field (union msgs (get struc field)))
  (make-propns struc field msgs))

; ******************************
; CLEAR-STRUC removes from memory an existing structure.

; specify personal-name struc by hand
(defun clear-struc (name)
  (setf *the-person* name) ; undo before returning?
  (remprop name 'constraint-hyps) ; if this hasn't been removed
  (do ((fields (get name 'fields) (cdr fields)))
      ((null fields) nil)
    (remprop name (car fields)))
  (remprop name 'fields)
  (remprop name 'data-type)
  (mapcar #'clear-propn (get name 'propositions))
  (remprop name 'propositions)
  (setf (get *the-person* 'all-structures) (remove name (get *the-person* 'all-structures)))
  (unless *silent-run?* (my-print '"Structure cleared: " name)))

; *THE-PERSON* MUST BE SET PROPERLY
(defun clear-propn (propn)
  (let ((struc (caar (get propn 'belongs-to)))
        (pred (pred-from-propn propn)))
    (remprop propn 'belongs-to)
    (remprop propn 'constraint-hyps)
    (setf (get *the-person* 'all-propositions) (remove propn (get *the-person* 'all-propositions)))
    (put pred 'from-propns (remove propn (get pred 'from-propns)))
    (if (put pred 'belongs-to (remove struc (get pred 'belongs-to)))
        nil (clear-pred pred)))
  (remprop propn 'message))

; *THE-PERSON* MUST BE SET PROPERLY
(defun clear-pred (pred)
  (remprop pred 'constraint-hyps)
  (remprop pred 'from-propns)
  (remprop pred 'belongs-to)
  (setf (get *the-person* 'all-preds) (remove pred (get *the-person* 'all-preds))))

; ******************************

; Convention: propositions are names like P1. Messages are
; lists: (predicate (arguments) proposition-name)

; PROPNS-FROM-STRUC
(defun propns-from-struc (struc)
  (get struc 'propositions))

; GET-PROPN-NAME
(defun get-propn-name (message)
    (car (last message)))

; PERSONAL-GET-PROPN-NAME
(defun personal-get-propn-name (message)
  (generic-to-personal-sym (get-propn-name message)))

; GET-PRED
(defun get-pred (message)
  (car message))

; PERSONAL-GET-PRED
(defun personal-get-pred (message)
  (generic-to-personal-sym (car message)))

; PRED-FROM-PROPN
(defun pred-from-propn (propn)
  (get-pred (get propn 'message)))

; PERSONAL-PRED-FROM-PROPN -added by MA
(defun personal-pred-from-propn (propn)
  (generic-to-personal-sym (get-pred (get propn 'message))))

; GET-ARGS
(defun get-args (message)
  (mapcar #'remove-hypothet (second message)))

; PERSONAL-GET-ARGS
(defun personal-get-args (message)
  (mapcar #'generic-to-personal-sym (get-args message)))

; REMOVE-HYPOTHET
(defun remove-hypothet (arg)
  (if (atom arg) arg (car arg)))

; ARGS-FROM-PROPN
; 10-03-2011 changed personal-get-args back to get-args, with corresp changes elsewhere -MA
(defun args-from-propn (propn)
  (get-args (get propn 'message)))

; PERSONAL-ARGS-FROM-PROPN  -MA 10-03-2011 not currently needed - is previous version of args-from-propn
(defun personal-args-from-propn (propn)
  (personal-get-args (get propn 'message)))

; GET-TR-VAL
(defun get-tr-val (message)
  (third message))

; TV-FROM-PROPN
(defun tv-from-propn (propn)
  (get-tr-val (get propn 'message)))

; UNIT?
; Is this a network node?
; Note it need not have any links.  Also, it's desirable to have a test that works
; before the unit is fully initialized, so that we can tell whether something's
; a unit early in the process of setting up the network.  e.g. the activation is set
; too late for some purposes.
(defun unit? (sym)
  (get sym 'belongs-to))

; *****************************

; CONC-FROM-STRUC lists all the concepts (i.e. PREDICATES) in a structure.
; 10-03-2011 changed personal-pred-from-propn back to pred-from-propn
;   after I changed a corresponding line in make-propns -MA
(defun conc-from-struc (struc)
  (remove-duplicates (mapcar #'pred-from-propn (get struc 'propositions))))

; OBJ-FROM-STRUC
(defun obj-from-struc (struc)
  (union-map #'args-from-propn (get struc 'propositions)))

; STRUC-FROM-PROPN returns the structure indexed by proposition number
(defun struc-from-propn (propn)
  (caar (get propn 'belongs-to)))

; *****************************
; MAKE-PROPNS sets up propositions.
; *THE-PERSON* MUST BE SET PROPERLY
(defun make-propns (struc field lst-of-messages)
  (mapc #'(lambda (msg) (make-propn struc field msg)) lst-of-messages))

; MAKE-PROPN
; Aside from calling note-unit, this does nothing but set data structures.
; *THE-PERSON* MUST BE SET PROPERLY or person must be passed
(defun make-propn (struc field msg &optional person)
  (let ((*the-person* (or person *the-person*))) ; if person is passed, temporarily set *the-person* to it [affects calls as well--defvar vars are dynamic]
    (let ((personal-msg (personalize-tree (remove-non-propn-elts msg))) ; personalize-tree is in popco.lisp or popco-utils.lisp
          (propn (get-propn-name msg))                    ; interpersonal canonical proposition symbol
          (personal-propn (personal-get-propn-name msg))  ; mentalese propositional symbol
          (pred (get-pred msg)))                          ; interpersonal predicate
      (when (includes-credence msg)                       ; added by MA 9/2011
        (note-unit personal-propn (get-tr-val msg)))    ; record credence as activation, if exists
      (put propn 'message msg)           ; give interpersonal prop pointer to structured prop
      (put personal-propn 'message personal-msg)  ; give mentalese prop link to structured prop (interpersonal format)
      (put personal-propn 'belongs-to
           (cons-if-new (list struc field) (get personal-propn 'belongs-to)))
      (put personal-propn 'is-causal (or    ; note we're using the interpersonal predicate to set personal properties [ADDED 7/14/2013 -MA]
                                       (put personal-propn 'is-causal-conditional (and (member pred *causal-if-preds*) t))      ; put returns the newly set value
                                       (put personal-propn 'is-causal-biconditional (and (member pred *causal-iff-preds*) t)))) ; 'and' outputs t rather than rest of list--less confusing
      (put personal-propn 'is-preventative (and (member pred *preventative-preds*) t))
      (setf (get *the-person* 'all-propositions)
            (cons-if-new personal-propn (get *the-person* 'all-propositions)))
      (put struc 'propositions
           (cons-if-new personal-propn (get struc 'propositions)))  ; changed propn to personal-propn -MA 10-03-2011
      (put (get-pred personal-msg) 'belongs-to
           (cons-if-new struc (get (get-pred personal-msg) 'belongs-to)))
      (put (get-pred personal-msg) 'from-propns
           (cons-if-new personal-propn (get (get-pred personal-msg) 'from-propns))) ; ??
      ;(format t "~%make-propn: activation of propn ~S is ~S" propn (activation propn)) ; DEBUG
      (setf (get *the-person* 'all-preds)  ; Q: IS THIS REDUNDANT GIVEN CONSTRAINT-MAP? -MA 11/2011
            (cons-if-new (get-pred personal-msg)
                         (get *the-person* 'all-preds))))))


(defun propns-of-struc (struc)
  (get struc 'propositions))

;; is a causal predicate
(defun causal-p (propn)
  (get propn 'is-causal))

;; note prevention is a subcategory of causation. 
;; to get non-preventative causal predicates, use 'not'
(defun preventative-p (propn)
  (get propn 'is-preventative))

;; subcategory of causal preds
(defun causal-conditional-p (propn)
  (get propn 'is-causal-conditional))

;; subcategory of causal preds
(defun causal-biconditional-p (propn)
  (get propn 'is-causal-biconditional))

; ******************************
; MCON (formerly MAKE-CONCEPT-A) is a bit different from make-concept in PI.
; As for structures, fields are: ((field-name value) ...)
; Typical structure will have some of:
; Concept name:
; 1. data-type: concept
; 2. superordinates: lst
; 3. subordinates: lst
; 4. part-of: lst
; 5. sub-parts: lst
; 6. decomp: structured list, representing semantic decomposition
; (maybe this should just be *synonyms*?)
; 7. rules: lst of rule-names
; Add to this: antonyms. Use when propositions are negated.

; There is some extra garbage around this stuff to provide a switch between
; the old version (symmetric-concepts) and the new.

(if *symmetric-concepts*
    (defun mcon (name fields &optional syntax)
      (put name 'data-type 'concept)
      (put name 'explicit t)
      (setf (get *the-person* 'all-concepts) (cons-if-new name (get *the-person* 'all-concepts)))
      (do ((flds fields (cdr flds)))
          ((null flds) (unless *silent-run?* (my-print '"Concept made: " name)))
        (put name
             (caar flds)
             (union (get name (caar flds)) (second (car flds))))
        (note-features name (caar flds) (second (car flds))))
      (if syntax (put name 'syntax syntax)))
  (defun mcon (name fields &optional syntax)
    (put name 'data-type 'concept)
    (put name 'explicit t)
    (setf (get *the-person* 'all-concepts) (cons-if-new name (get *the-person* 'all-concepts)))
    (do ((flds fields (cdr flds)))
        ((null flds) (unless *silent-run?* (my-print '"Concept made: " name)))
      (put name
           (caar flds)
           (union (get name (caar flds)) (second (car flds))))
      (cond ((eq (caar flds) 'tenses)
             (do ((feats (second (car flds)) (cdr feats)))
                 ((null feats) nil)
               (put (car feats) 'root-tense (list name))))
            ((eq (caar flds) 'plural)
             (do ((feats (second (car flds)) (cdr feats)))
                 ((null feats) nil)
               (put (car feats) 'singular (list name))))))
    (if syntax (put name 'syntax syntax))))

; ******************************
; NOTE-FEATURES sets up associations from feature to concepts

(defun note-features (conc field lst)
  (unless (eq field 'rules)
    (associate-conc conc field lst))
  (if (and (eq field 'rules)
           (eq *feature-selection* 'loose))
      (associate-conc conc 'rules (concs-from-rules lst))))

; ******************************
; ASSOCIATE-CONC puts the current concept into the appropriate
; field of each feature.

(defun associate-conc (conc field feats)
  (let (thisfield)
    (cond
     ((equal field 'synonyms) (setf thisfield 'synonyms))
     ((equal field 'antonyms) (setf thisfield 'antonyms))
     ((equal field 'superordinates) (setf thisfield 'subordinates))
     ((equal field 'subordinates) (setf thisfield 'superordinates))
     ((equal field 'sub-parts) (setf thisfield 'part-of))
     ((equal field 'part-of) (setf thisfield 'subparts))
     ((equal field 'tenses) (setf thisfield 'root-tense))
     ((equal field 'plural) (setf thisfield 'singular))
     ((equal field 'variations) (setf thisfield 'root-conc)))
    (do ((lst feats (cdr lst)))
        ((null lst) 'done)
      (put (car lst) thisfield
           (cons-if-new conc (get (car lst) thisfield)))
      (setf (get *the-person* 'all-concepts) (cons-if-new (car lst) (get *the-person* 'all-concepts))))))

; *************************
; CONSTRAINT-MAP uses a constraint network to do analogical
; mapping between two problems.
; [MA: I believe the args are treated entirely symmetrically--you can swap the two
; arguments, and the network structure, settled state, etc. will be the same,
; though the strings in units' symbol names on either side of the "=" will
; be reversed.  However, HOLYOAK AND THAGARD 1989 APPARENTLY USE TARGET, SOURCE AS
; THE USUAL ORDER, where target describes the domain about which you want to
; learn, make a decision, solve problem, fill in information, etc.  This can
; be inferred from the ordering of strings in the X=Y symbols on page 309, and
; in the PRESUMED specification on page 331.
; (Note however that some functions in acme-infer.lisp are assymetric: They add
; nodes to the target structure based on what's in the source structure, but
; not vice versa.) -MA 11/2011]
; *THE-PERSON* MUST BE SET PROPERLY
(defun constraint-map (personal-struc1 personal-struc2)  ; note the args must already be converted to personal analog struc names
  (unless *silent-run?* (my-print '"Constructing analogical mapping between "
                                  personal-struc1 '" and " personal-struc2 
                                  " for person " *the-person*))

  ; Find and store all of the predicates used by person's propositions:
  ; note objects and concepts for sake of clear-net:
  ; the objects are now taken care of in make-obj-unit - GHN 6/21/88
  ; Q: IS THIS REDUNDANT GIVEN MAKE-PROPNS?  -MA 11/2011 ; IS IT INEFFICIENT?
  (setf (get *the-person* 'all-preds) (union (conc-from-struc personal-struc1)
                                             (conc-from-struc personal-struc2)))
  ; note: (get *the-person* 'all-propositions) set up by make-struc
  ; set up the network:
  (make-constraint-net personal-struc1 personal-struc2)
  (if *use-auto-prag?* (check-importance personal-struc1)))

; **************************

; PROPNS-FROM-MSGS pulls out proposition names - last part of message.

(defun propns-from-msgs (msgs)
  (mapcar #'last-element msgs))

(defun last-element (lst) (car (last lst)))

(defmacro is-new-unit (unit) `(get ,unit 'is-newly-added))

(defun mark-propn-unit-newly-added (unit person)
  (unless (is-new-unit unit)
    (push unit (get person 'newly-added-propn-units))
    (setf (is-new-unit unit) t)))

(defun mark-map-unit-newly-added (unit person)
  (unless (is-new-unit unit)
    (push unit (get person 'newly-added-map-units))
    (setf (is-new-unit unit) t)))

;; NOTE DOES NOT REMOVE FROM new lists (which would be inefficient)
;; instead zero out the list separately.
(defun mark-unit-old (unit)
  (setf (is-new-unit unit) nil))

;; To avoid pushing multiple copies of the same constraint,
;; test for oldness outside of this function, e.g. using weight-of-link-between = 0.
;; This allows slightly better efficient in make-symlink. Maybe not worth it ....
(defun mark-constraint-newly-added (unit1 unit2 weight person)
  ;(format t "mark-constraint-newly-added: ~S ~S ~S ~S~%" unit1 unit2 weight person) ; DEBUG
  (push `(,unit1 ,unit2 . ,weight) (get person 'newly-added-constraints)))

(defun mark-contraint-newly-removed (unit1 unit2 person)
    (push `(,unit1 ,unit2) (get person 'newly-removed-constraints))) ; don't need weight

(defun remove-constraint (unit1 unit2)
  (let ((person (or (person-of unit1)      ; use OR because one unit might be a special unit such as 'SALIENT
                    (person-of unit2))))   ;  [assuming no special-to-special links and no inter-personal links (no telepathy allowed)]
    (mark-contraint-newly-removed unit1 unit2 person)
    (unlink-units unit1 unit2)))

(defun remove-all-constraints-from (unit)
  (mapc #'(lambda (half-link) (remove-constraint (car half-link) unit))
        (get unit 'links-from)))

; *************************
; MAKE-CONSTRAINT-NET sets up a network of hypotheses
; based on two structures that may be analogous.
; Each structure can have any number of fields.

; *THE-PERSON* MUST BE SET PROPERLY
(defun make-constraint-net (struc1 struc2)
  (let (links-num)
    (unless *silent-run?* (my-print '"Constructing constraint network for "
                                    struc1 '" and " struc2))
    (setf *struc1* struc1)
    ; set up units and excitatory links for each field:
    
    (make-units-for-fields struc1 struc2)
    
    (unless *silent-run?*
      (and (my-print " Total number of units made: " (length (get *the-person* 'all-units)))
           (my-print " Total number of symmetric excitatory links: "
                     (/ (get *the-person* 'total-links) 2))))
    (setf links-num (get *the-person* 'total-links))
    ; set up links for queries:
    (if *look-for-queries?* (mapcar #'make-query-links *query-connections*))
    
    ; set up inhibitory links among concept hypotheses:
    (unless *silent-run?* (my-print " Making inhibitory links ..."))
    (inhibit-multiple-mappings (conc-from-struc struc1) *inhib-weight*)
    
    ; for 1-1 mapping:
    (if *map-one-one?* (inhibit-multiple-mappings (conc-from-struc struc2)
                                                  (* *stop-many-one* *inhib-weight*)))

    ; inhibitory links among object hypotheses (can be less than concepts)
    (inhibit-multiple-mappings (obj-from-struc struc1)
                               (* *inhib-weight* *obj-conc-fraction*))
    ; for 1-1 mapping:
    (if *map-one-one?* (inhibit-multiple-mappings (obj-from-struc struc2)
                                                  (* *inhib-weight* *obj-conc-fraction*)))
    ; inhibitory links to enforce 1-1 mapping of propositions:
    (inhibit-multiple-mappings (get *the-person* 'all-propositions)
                               (* *inhib-weight* *propn-uniqueness*))
    (unless *silent-run?*
      (and (my-print " Symmetric inhibitory links made: "
                     (/ (- (get *the-person* 'total-links) links-num) 2))
           (my-print '" Total symmetric links made: "
                     (/ (get *the-person* 'total-links) 2))))
    (length (get *the-person* 'all-units)))); end let

; *************************
; MAKE-UNITS-FOR-FIELDS constructs units excitatory links
; for proposition mappings in particular fields.
; It ignores the different fields, combining them all
; into one big field, if map-all? is t.

; *THE-PERSON* MUST BE SET PROPERLY
(defun make-units-for-fields (struc1 struc2)
  (if *map-all?*
      (and (combine-fields struc1)
           (combine-fields struc2)))
  (do ((fields (get struc1 'fields) (cdr fields))
       (unit-num (length (get *the-person* 'all-units)))
       (link-num (get *the-person* 'total-links)))
      ((null fields) 'done)
    ; repeat:
    (setf unit-num (length (get *the-person* 'all-units)))
    (setf link-num (get *the-person* 'total-links))
    (unless *silent-run?*
      (my-print " Making units and excitatory links for field "
                (car fields) " ..." ))
    (make-hyp-units struc1 struc2 (car fields))
    (unless *silent-run?*
      (and (my-print " Units made: "
                     (- (length (get *the-person* 'all-units)) unit-num))
           (my-print " Symmetric links made: "
                     (/ (- (get *the-person* 'total-links) link-num) 2))))))

; *************************
; COMBINE-FIELDS conglomerates different fields into one.

(defun combine-fields (struc)
  (do ((fields (get struc 'fields) (cdr fields))
       (result nil))
      ((null fields)
       (put struc 'all result))
    (setf result (union result
                        (get struc (car fields)))))
  (put struc 'fields '(all)))


; *************************
; MAKE-HYP-UNITS sets up units representing hypotheses that
; predicates and their associated objects are analogous.
; New: also does propositions.

(defun make-hyp-units (prob1 prob2 field)
  (do ((msgs (get prob1 field) (cdr msgs)))
      ; exit:
    ((null msgs) 'done)
    ; repeat:
    (make-hyp-units-for-msg (car msgs)
                            (get prob2 field))))

; ****************************
; MAKE-HYP-UNITS-FOR-MSG makes units for a particular message.

(defun make-hyp-units-for-msg (msg messages)
  ; set up null mappings:
  (if *use-nothing-maps?*
      (make-hyp-unit msg
                     (list 'no-concept
                           (list-of-n-elem (length (get-args msg))
                                           'no-object)
                           'no-proposition)))
  ; all the rest:
  (do ((msgs messages (cdr msgs)))
      ; exit:
    ((null msgs) 'done)
    ; repeat:
    (make-hyp-unit msg (car msgs))))



; ****************************
; MAKE-HYP-UNIT creates units corresponding to the hypotheses
; that two messages are analogous, i.e. that their concepts, arguments,
; and whole propositions correspond.
; Returns a list of the concept and proposition mappings created.
; Only concepts with same # of arguments are candidates.
; Structure of units:
; Unit-name
; Activation: -1 ... 1
; Linked-from: list of units.

;;; The new version checks to see if new-propn-unit already exists.

; PT 5-22-98 Modify mapping in line with DRAMA, so that on
; the first pass only semantically similar concepts are mapped;
; then, on the second pass, unmapped propositions can be
; mapped without this restriction, to ensure that everything
; in the source has a chance to map.

; *THE-PERSON* MUST BE SET PROPERLY
(defun make-hyp-unit (msg1 msg2)
  ;(when (equal msg1 msg2) (error "make-hyp-unit: ~S is ~S" msg1 msg2)) ; DEBUG
  ;(format t "in make-hyp-unit:~%msg1: ~S~%msg2: ~S~%" msg1 msg2) ; DEBUG
  (let* ((conc1 (car msg1))
         (conc2 (car msg2))
         (personal-conc1 (generic-to-personal-sym conc1))
         (personal-conc2 (generic-to-personal-sym conc2))
         (propn1 (last-element msg1))
         (propn2 (last-element msg2))
         (personal-propn1 (generic-to-personal-sym propn1))
         (personal-propn2 (generic-to-personal-sym propn2))
         (args1 (personal-get-args msg1)) ; Note there's a later pair of calls to
         (args2 (personal-get-args msg2)) ; plain vanilla get-args in this function.
         (new-conc-unit nil)
         (new-propn-unit nil)
         (object-units nil)
         (result nil)
         (sem-sim? nil))
    (cond ((and *ignore-preds*
                (member conc1 *ignore-preds*)
                (member conc2 *ignore-preds*)))  ; if test satisfied, do nothing
          ; most of the rest of make-hyp-unit is conditioned on the test in next line
          ((isomorphic-arglists args1 args2) ; This behaves like the PT code it replaces iff +deep-isomorphic-matching+ is nil
           ;(format t "make-hyp-unit firing on ~S ~S~%" personal-propn1 personal-propn2) ; DEBUG
           (unless *silent-run?*
             (my-print "make-hyp-unit: Mapping: " msg1)
             (my-print "                    and " msg2))
           (setf new-conc-unit (catname conc1 conc2))
           (setf (get new-conc-unit 'unit-type) 'acme) ; added 6/2011 to identify map units as such -MA  ; **SHOULDN'T THIS BE IN THE COND BELOW?**
           ; if concept unit not already made, create it:
           (cond ( (not-member new-conc-unit (get *the-person* 'all-units)) ; cond 2
                  (mark-map-unit-newly-added new-conc-unit *the-person*) ; MA 4/2012 replaces old pushnew line
                  ; set up unit
                  (put new-conc-unit 'concerns (list personal-conc1 personal-conc2))
                  ; note creation of unit:
                  (note-unit new-conc-unit)
                  (pushnew new-conc-unit (get *the-person* 'all-map-units))
                  (setf result (cons new-conc-unit result))
                  ; calculate semantic similarity of concepts
                  ;(format t "conc1: ~S; conc2: ~S~%" conc1 conc2)
                  (setf sem-sim?
                        (if *use-arcs-semantics?*
                          (sem-sim-arcs conc1 conc2)    ; s/b personal-conc? 
                          (sem-similarity conc1 conc2)))
                  ;(format t "sem-sim? for ~S and ~S is: ~S" conc1 conc2 sem-sim?) (if (/= sem-sim? 0) (format t " ... making symlink~%") (format t "~%")) ; DEBUG
                  (if (/= sem-sim? 0)                                   ; if same preds, or marked as similar,
                    (make-symlink 'special new-conc-unit sem-sim?))  ; then link the pred map node to special
                  ; record hypotheses about a concept:
                  (record-hypothesis personal-conc1 new-conc-unit)
                  (record-hypothesis personal-conc2 new-conc-unit))); end cond 2
           ; create proposition unit:
           (setf new-propn-unit (catname propn1 propn2))
           (setf (get new-propn-unit 'unit-type) 'acme) ; added 6/2011 to identify map units as such -MA
           ;;; The cond around the following four lines is the fix that Eric Melz
           ;;; devised -- installed by Greg Nelson 12/07/89
           (cond ((not-member new-propn-unit (get *the-person* 'all-units))
                  ;(format t "make-hyp-unit firing on ~S ~S~%" personal-propn1 personal-propn2) ; DEBUG
                  (put new-propn-unit 'concerns (list personal-propn1 personal-propn2))
                  (note-unit new-propn-unit)
                  (mark-map-unit-newly-added new-propn-unit *the-person*) ; MA 4/2012 replaces old pushnew line
                  (pushnew new-propn-unit (get *the-person* 'propn-map-units)) ; added by MA 11/2011 [so won't have to search through all-units when updating propn-propn links]
                  (pushnew new-propn-unit (get *the-person* 'all-map-units))
                  (record-hypothesis personal-propn1 new-propn-unit)
                  (record-hypothesis personal-propn2 new-propn-unit)
                  (setf result (cons new-propn-unit result))))  ; This is never used? -MA 6/2011
           ; link proposition unit with concept unit:
           (make-symlink new-propn-unit new-conc-unit *excit-weight*)

           ; if msg2 from target contains a query, treat specially:
           (cond ( (and *look-for-queries?* ; cond 3
                        (contains-query (get-args msg2)))  ; SHOULD BE PERSONAL-GET-ARGS?
                  (setf *query-connections*
                        (cons (list new-propn-unit
                                    new-conc-unit
                                    (get-args msg1)   ; SHOULD BE PERSONAL-GET-ARGS?
                                    (get-args msg2))  ; SHOULD BE PERSONAL-GET-ARGS?
                              *query-connections*)))
                 ; else make excitatory links with objects:
                 (t (setf object-units (make-obj-units (get-args msg1) ; Vanilla get-args
                                                         (get-args msg2) ; is correct here.
                                                         *init-activ*))
                    (make-excit-links new-propn-unit  ; make-excit-links in echo.lisp just iterates make-symlink
                                      object-units
                                      *excit-weight*)
                    (if *link-concepts-objects?*
                      (make-excit-links new-conc-unit  ; see comment immediately above
                                        object-units
                                        *excit-weight*)))) ; end cond 3
           ) ; end of cond 1 condition's internal progn
    ) ; end of cond 1
  ) ; end of let
)

; **********************************
; TYPE-COMPATIBLE ensures for all members of two sequences (A B C) and (1 2 3)
; that if A is a member of a set, then so is 1.
; NOTE: Although this function is written as a general utility, in the 2010 Thagard
;       website COHERE code it was only used in make-hyp-unit to test whether argument lists
;       for two proposition messages are compatible in the sense that they have objects 
;       and propositions in the same positions.  [In principle it t might also have been 
;       used in make-obj-unit for the same purpose, but wasn't.] -MA 3/2012
(defun type-compatible (lst1 lst2 set)
  (do ((seq1 lst1 (cdr seq1))
       (seq2 lst2 (cdr seq2)))
      ; if everything ok, return t.
    ((null seq1) t)
    ; check all pairs:
    (if (or (and (member (car seq1) set)
                 (not-member (car seq2) set))
            (and (not-member (car seq1) set)
                 (member (car seq2) set)))
        (return nil))))

; SHALLOW-ISOMORPHIC-ARGLISTS
; Wrapper for PT's type-compatible function, incorporating the length test
; which formerly preceded it in make-hyp-unit.
; Differs from isomorphic-args in that it only tests whether there are objects
; and propositions in the same places in arglists of the same length; doesn't
; check whether propositions in the same places themselves have isomorphic arglists.
; NOTE: *THE-PERSON* must be set correctly
(defun shallow-isomorphic-arglists (args1 args2)
  (and (= (length args1) (length args2))
       (type-compatible args1 args2 (get *the-person* 'all-propositions))))

; Note: as of 4/1/2012, deep-isomporphic-arglists is significantly faster than 
; shallow-isomorphic-arglists (after the initial net setup).  
; The latter can probably be sped up if
; (The difference is > 1 second for 10 persons in 30 five-cycle pop-ticks with
; no conversation, calling one of these functions once in run-hyp-units, running
; parenting14c.lisp in SBCL 1.0.53 on a 1.6ghz 4MB MacBook Air runing OS X 1.6.)
; That's out of 8 or 9 seconds, so > 10% difference.
; Hmm I suppose some of that speedup might just be that it means that
; fewer propns run through the rest of make-hyp-unit.

; DEEP-ISOMORPHIC-ARGLISTS
; New, more-restrictive version of type-compatibility test.  Note no 3rd arg.  - MA 3/30/2012
; Unlikely PT version, this one recursively tests whether proposition's args themselves have 
; compatible arglists if they are propositions.  i.e. higher-order propositions args must be
; type-compatible in order for the h-o propns to be type-compatible.  This allows ACME to
; be senstitive to more of the structural relationships between two analogs.
; [Not sure whether this is good idea, though, since choice of number of arguments is a
; somewhat arbitrary decision on the part of the modeler.  So use of this test imposes a
; further requirement on the modelers to make sure that propositions that should be capable
; of mapping satisfy this test.]
; NOTE: ASSUMES THAT ALL ARG TREES BOTTOM OUT IN OBJECTS:
;	i.e. it's possible to generate infinite recursion, e.g. by trying to match the args of:
;	        (f (g-propn) f-propn)
;	        (g (f-propn) g-propn)
;	We could add a test to this function to prevent this, but doesn't seem worth it yet.
(defun deep-isomorphic-arglists (args1 args2)
  (cond ((and (null args1) (null args2)) t)  ; if both lists empty: compatible
        ((or (null args1) (null args2)) nil) ; if diff lengths: not compatible
        (t (and (isomorphic-args (car args1) (car args2))         ; are first elems compat?
                (deep-isomorphic-arglists (cdr args1) (cdr args2)))))) ; if so, then check rest

(defun isomorphic-args (arg1 arg2)
  (cond ((and (propn? arg1) (propn? arg2)) ; if both are propns, then
         (deep-isomorphic-arglists           ; recursively check whether arglists are compatible
           (args-from-propn arg1) 
           (args-from-propn arg2)))
        ((or (propn? arg1) (propn? arg2)) nil) ; if one is propn, other isn't: not compatible
        (t t))) ; otherwise both args are presumably objects, so are compatible


; **********************************
; RECORD-HYPOTHESIS notes that a unit provides one of the possible
; mappings for concept, object, or proposition.

(defun record-hypothesis (thing unit)
  (put thing 'constraint-hyps
       (cons-if-new unit (get thing 'constraint-hyps))))

; *******************************
; CONTAINS-QUERY checks whether a list contains a query to be
; filled in, as indicated by an argument that starts with a "?".

(defun contains-query (list-of-atm)
  (do ((lst list-of-atm (cdr lst))
       (result nil))
      ; exit:
    ((null lst) result)
    ; repeat:
    (if (atom-begins (car lst) #\?)
        (setf result (cons (car lst) result)))))

; *******************************
; MAKE-QUERY-LINKS sets up links based on a queries.
; The basic idea here is that units that provide a chance of
; answering a query will be favored over those that don't.
; [NOTE I think it's possible for this function to cause
; make-obj-unit to create proposition-map units for propositions 
; with incompatible argument lists--e.g. different numbers of
; arguments--whether +deep-isomorphic-matching+ is nil or t. -MA 4/20120]
(defun make-query-links (lst-of-four)
  (let ((propn-unit (car lst-of-four))
        (conc-unit (second lst-of-four))
        (arguments1 (third lst-of-four))
        (arguments2 (fourth lst-of-four)))
    
    (unless *silent-run?*
      (my-print '"Making query links: " propn-unit " " conc-unit '" "
                arguments1 '" " arguments2))
    (do ((args1 arguments1 (cdr args1))
         (args2 arguments2 (cdr args2))
         (result-args1 arguments1)
         (result-args2 arguments2)
         (new-obj-units nil)
         (query-args nil))
        ; exit:
      ((null args1)
       ; make normal object units:
       (setf new-obj-units
             (make-obj-units result-args1
                             result-args2
                             *init-activ*))
       (cond ((and *look-for-queries?* (not *silent-run?*))
              (my-print '"DEBUGGING make-query-links ")
              (my-print '"new-obj-units " new-obj-units)
              (my-print '"result args " result-args1 result-args2)))
       ; make normal links:
       
       (make-symlink propn-unit
                      conc-unit
                      *excit-weight*)
       (make-excit-links propn-unit
                         new-obj-units
                         *excit-weight*)
       ; make special query links:
       (make-query-links-for-objs query-args
                                  (cons conc-unit new-obj-units)))
      ; repeat:
      (cond ( (atom-begins (car args2) #\?)
             (setf query-args (cons (car args1) query-args))
             (setf result-args1 (remove (car args1) result-args1))
             (setf result-args2 (remove (car args2) result-args2))
             ))) ; end do
    ) ; end let
  )

; *******************************
; MAKE-QUERY-LINKS-FOR-OBJS makes links among each member of a set of
; constraint hypotheses (e.g. all hypotheses concerning obj1)
; and the pragmatic unit.

(defun make-query-links-for-objs (list-of-objects units)
  (create-pragmatic-unit)
  (do ((objs list-of-objects (cdr list-of-objects)))
      ; exit:
    ((null objs) 'done)
    ; repeat:
    (make-query-links-for-obj (car objs) units)))

; *******************************
; MAKE-QUERY-LINKS-FOR-OBJ does it for one object.

(defun make-query-links-for-obj (obj units)
  (do ((unts units (cdr unts)))
      ; exit
    ((null unts) 'done)
    ; repeat:
    (make-excit-links 'pragmatic
                      (get obj 'constraint-hyps)
                      *import-weight*)
    (unless *silent-run?*
      (my-print "Units important for querying: "
                (get obj 'constraint-hyps)))))

; *****************************
; NUMBER-DUPS looks for pairs of duplicate arguments: e.g.
; (a b a) and (d e d). Not quite perfect. [MA: Right--results are pretty quirky.]

(defun number-dups (list1 list2)
  (do ((lst (make-pairs list1 list2) (cdr lst))
       (result 1))
      ; exit:
    ((null lst) result)
    ; repeat:
    (if (memberlist (car lst) (cdr lst))
        (setf result (+ result 1)))))

; *******************************
; MAKE-PAIRS takes two lists, and returns a list of pairs,
; with the nth atom of list1 paired with the nth of list2.
; [from pi/analog.l].

(defun make-pairs (lst1 lst2)
  (cond ( (not-equal (length lst1) (length lst2)) nil)
        ( (null lst1) nil )
        ( t (cons (list (car lst1) (car lst2))
                  (make-pairs (cdr lst1) (cdr lst2))))))

; ******************************
; SEM-SIMILARITY calculates the semantic similarity between
; two concepts. It should use an overlapping feature count,
; but see particular data files for *no-concept-weight*s.
; PT, 5-95. For ACME in COHERE, don't do full ARCS similarity
; judgment, but just look for identical or similar predicates.

(defun sem-similarity (conc1 conc2)
  ;(format t "~%Checking similarity of ~S and ~S: ~S~%" conc1 conc2 (similar?  conc1 conc2)) ; DEBUG -MA 3/2012
  (cond ( (equal conc1 conc2) *ident-weight*) ; concepts are the same
        ( (equal conc2 'no-concept) *no-concept-weight*) ; null mapping
        ( (similar? conc1 conc2) (similar? conc1 conc2)) ; concepts similar
        (t *no-sim-weight*)))

; SIMILAR ; fixed 11-98 PT
;(defvar *similarities* nil)

; *THE-PERSON* must be set properly.
(defun similar (conc1 conc2 val)
  ;(format t "~%Setting ~S and ~S to be similar to degree ~S.~%" conc1 conc2 val) ; DEBUG -MA 3/2012
  (setf (get *the-person* 'similarities) 
        (cons (list (list conc1 conc2) val) 
              (get *the-person* 'similarities))))


; SIMILAR? checks a person's similarities list for a measure of
; the similarity of two concepts.
; *THE-PERSON* must be set properly.
(defun similar? (conc1 conc2)
  (do ((sims (get *the-person* 'similarities) 
             (cdr sims)))
      ((null sims) nil)
    ; repeat
    (if (or (equal (list conc1 conc2) (caar sims))
            (equal (list conc2 conc1) (caar sims)))
        (return (second (car sims))))))

; NOSIMILAR

(defun nosimilar ()
  (setf *no-concept-weight* nil))


; Here's how it should be if Wordnet were comprehensive enough:
; SEM-SIMILARITY-FUTURE calculates the semantic similarity between
; two concepts. Typically, this should be:
; identical: .1
; synonymous: .08
; coordinates: .06 (i.e. are both kinds of same superordinate,
; or parts of same whole)

(defun sem-similarity-future (conc1 conc2)
  (cond ( (eq conc1 conc2) *ident-weight*) ; concepts are the same
        ( (eq conc2 'no-concept) *no-sim-weight*) ; null mapping
        ( (synonymous conc1 conc2) *synon-weight*)
        ( (coord conc1 conc2) *coord-weight*)
        (t *no-sim-weight*)))
; *************************
; SYNONYMOUS

(defun synonymous (conc1 conc2)
  (or (memberlist (list conc1 conc2) *synonyms*)
      (memberlist (list conc2 conc1) *synonyms*)))

; COORD identifies concepts that are parts of the same whole or
; subkinds of the same kind.

(defun coord (conc1 conc2)
  (or (memberlist (list conc1 conc2) *same-kinds*)
      (memberlist (list conc2 conc1) *same-kinds*)
      (memberlist (list conc1 conc2) *same-parts*)
      (memberlist (list conc2 conc1) *same-parts*)))

;; THE NEXT TWO FUNCTIONS DON'T SEEM TO BE CALLED ANYWHERE
;; so I'm commenting them out for clarity until they're needed. -MA 4/2012
; 
;; *************************
;; MAKE-OBJ-UNITS-FROM-OBJECTS sets up hypotheses about
;; about object maps.
;
;(defun make-obj-units-from-objects (struc1-objs struc2-objs)
;  (do ((s1-objs struc1-objs (cdr s1-objs))
;       (units-made nil))
;      ; exit: return units made:
;    ((null s1-objs) units-made)
;    ; while repeating:
;    (setf units-made
;          (union units-made
;                 (make-obj-units-from-1-obj (car s1-objs)
;                                            struc2-objs)))))
;
;; **************************
;; MAKE-OBJ-UNITS-FROM-1-OBJ does it for 1.
;
;(defun make-obj-units-from-1-obj (obj objects)
;  (do ((obs objects (cdr obs))
;       (units-made (if *use-nothing-maps?*
;                       (list (make-obj-unit obj 'no-object *init-activ*)))
;                   ;else
;                   nil))
;      ; exit: return units
;    ((null obs) (remove nil units-made))
;    ; repeat:
;    (setf units-made
;          (cons (make-obj-unit obj (car obs) *init-activ*)
;                units-made))))

; **************************
; EXCIT-QUERIES sets up special excitatory links for
; query objects starting with "?".
; Excitatory links are set up between each query object mapping to
; an element and all units involving that element.

(defun excit-queries (quer)
  (do ((quer-maps (get quer 'constraint-hyps) (cdr quer-maps)))
      ((null quer-maps) 'done) ; (important (list quer)) - retards settling.
    ; repeat:
    (my-print (car quer-maps) " query map")
    (make-excit-links (car quer-maps)
                      (other-element-maps quer (car quer-maps))
                      *excit-weight*)
    (make-inhib-links (get quer 'constraint-hyps) *inhib-weight*)))

; *************************
; OTHER-ELEMENT-MAPS gives for an element E all
; the units concerning the mapping of all elements that
; map to E.

(defun other-element-maps (el unit)
  (remove unit
          (get (other-from-pair el
                                (get unit 'concerns))
               'constraint-hyps)))

; **************************
; To calculate the best match:
; **************************
; BEST-ANALOGY uses the hypothesis units to judge what is the
; best overall match:

; *THE-PERSON* MUST BE SET PROPERLY
(defun best-analogy (prob)
  (unless *silent-run?*
    (my-print '"Calculating the best mappings after "
              (get *the-person* 'total-times) '" cycles."))
  (setf *best-matches* nil)
  (mapcar #'best-match (union (conc-from-struc prob)
                              (obj-from-struc prob)))
  (if *stop-when-matched?*
      (cond ( (null (set-difference *desired-matches* *best-matches*
                                    :test #'equal))
             (setf *stop-run?* t)
             (unless *silent-run?*
               (my-print '"Desired match accomplished at cycle "
                         (get *the-person* 'total-times)))))))

(defun ba () (best-analogy *struc1*))

; **********************************
; FIND-ANALOG-MESSAGES constructs analogous messages.

(defun find-analog-messages (lst-of-preds prob part-of-prob)
  (do ((preds lst-of-preds (cdr preds)))
      ; exit:
    ((null preds) nil)
    ; repeat:
    (find-analog-message (car preds) prob part-of-prob)))



; **********************************
; FIND-ANALOG-MESSAGE constructs an analogous message for a predicate
; based on the best match of objects.

;; PROBABLY NEEDS PERSONALIZATION EG WITH PERSONAL-GET-ARGS
(defun find-analog-message (pred problem part-of-problem)
  (do ((msgs (get problem part-of-problem) (cdr msgs))
       (result nil))
      ; exit:
    ((null msgs) nil)
    ; repeat:
    (cond ( (> (sem-similarity pred (caar msgs)) *coord-weight*)
           (setf result (list (caar msgs)
                              (mapcar #'best-match (get-args (car msgs)))))
           (my-print '"Analogous message is " result)
           (return result)))))

; ********************************
; LIST-OF-N-ELEM makes a list consisting of n occurrences of an element.

(defun list-of-n-elem (number element)
  (do ((num number (- num 1))
       (lst nil))
      ((= 0 num) lst)
    (setf lst (cons element lst))))

; ******************************
; PURPOSE: contains functions shared by ACME and ARCS
; **************************
; PRINT-GOOD reports on goodness of fit of analogy.

; *THE-PERSON* MUST BE SET PROPERLY
(defun print-good ()
  (my-print '"Goodness of network: " (goodness (get *the-person* 'all-units))))

; *****************************
; MAKE-OBJ-UNITS makes hypotheses relating pairs of
; ordered sequences of arguments.

; *THE-PERSON* MUST BE SET PROPERLY
(defun make-obj-units (arguments1 arguments2 activation)
  (do ((args1 arguments1 (cdr args1))
       (args2 arguments2 (cdr args2))
       (units-made nil (cons (make-obj-unit (car args1)
                                            (car args2)
                                            activation)
                             units-made))
       ; adjust excitation weight for duplicate arguments: see data/num.l
       (weight (if *watch-for-dup-arguments?*
                   (* *excit-weight* (number-dups arguments1 arguments2))
                 *excit-weight*)))
      ((null args1)
       (setf units-made (remove-duplicates units-made))
       ;(setf *object-units* (union units-made *object-units*))  ; commented out - having no effect in POPCO -MA 4/2012
       (if *link-objects?* (make-all-excit-links units-made weight)) ; make-all-excit-links merely calls make-symlink on all poss pairs from units-made
       units-made)))

; *****************************
; MAKE-OBJ-UNIT makes a unit corresponding to the hypothesis that
; two objects are identical. Changed for MCL, PT, 5-95.
; [NOTE that if +deep-isomorphic-matching+ is nil, so that traditional COHERE
; type checking is used in make-hyp-unit, then when this function is (indirectly)
; called from make-hyp-unit, it can create map nodes between propositions
; without matching argument lists--e.g. with different numbers of arguments--
; even though make-hyp-unit can't do that.  The same is probably true
; even when +deep-isomorphic-matching+ is t, when this function is (indirectly)
; called from make-query-links. -MA 4/2012]
; *THE-PERSON* MUST BE SET PROPERLY
(defun make-obj-unit (obj1 obj2 activation)
  (let ((new-unit)
        (new-obj1)
        (new-obj2))
    (setf new-unit (catname obj1 obj2))
    (setf new-obj1 (generic-to-personal-sym obj1))
    (setf new-obj2 (generic-to-personal-sym obj2))

    (cond ((not-member new-unit (get *the-person* 'all-units))
           ;(format t "~%make-obj-unit: making ~S~%~%" new-unit) ; DEBUG
           (setf (get new-unit 'unit-type) 'acme) ; added 6/2011 to identify map units as such. -MA. Moved from a few lines above 3/31/2012
           (put new-unit 'concerns
                (list new-obj1 new-obj2))
           (note-unit new-unit) ; sets initial activation and records in 'all-units
           (push new-unit (get *the-person* 'all-map-units))
           (mark-map-unit-newly-added new-unit *the-person*) ; MA 4/2012 replaces old pushnew line
           (when (and (propn? new-obj1) (propn? new-obj2))
             (pushnew new-unit (get *the-person* 'propn-map-units))) ; added by MA 3/30/12 [if a propn map unit is created here, make-hyp-unit won't try to create it later]
           
           ; unless unit already got activation from make-hyp-unit
           (unless (get new-unit 'activation)              ; DIDNT NOTE-UNIT JUST SET THE ACTIVATION?
             (and (put new-unit 'activation activation)
                  (put new-unit 'original-activation activation)))
           (put new-obj1 'constraint-hyps
                (cons-if-new new-unit (get new-obj1 'constraint-hyps)))
           (put new-obj2 'constraint-hyps
                (cons-if-new new-unit (get new-obj2 'constraint-hyps)))
           (setf (get *the-person* 'all-objects) 
                 (union (get *the-person* 'all-objects) (list new-obj1 new-obj2)))
           new-unit)
          (t new-unit))))

; *************************
; INHIBIT-MULTIPLE-MAPPINGS sets up inhibitory links among competing
; hypotheses about a particular object or concept.
; Will not clobber excitatory links: see make-symlink.
; For pragmatics sake, it treats queries differently, forming
; excitatory rather than inhibitory links.

;;; This fixes a bug which mixes up source and target components when
;;; considering inhibitory connections. For example, num5=num3 was
;;; inhibited by num6=num5.

; *THE-PERSON* MUST BE SET PROPERLY
(defun inhibit-multiple-mappings (list-of-objs-or-concs weight)
  (do ((lst (remove-nil-dup list-of-objs-or-concs) (cdr lst))) ; remove-nil-dup removes nil's and duplicates
      ; exit:
    ((null lst) (get *the-person* 'total-links))
    (if (atom-begins (car lst) #\?) ; query
      (excit-queries (car lst))
      ; otherwise:
      ; make negatively-weighted links between map units for competing mapping hypotheses:
      (progn
       (make-inhib-links
        (no-queries ; strips out any queries
         (get-firsts (car lst) (get (car lst) 'constraint-hyps))) ; get map units in which obj-or-conc participates on the left side
        weight)
       (make-inhib-links
        (no-queries ; strips out any queries
         (get-seconds (car lst) (get (car lst) 'constraint-hyps))) ; get map units in which obj-or-conc participates on the right side
        weight)))))

; The original, old version, which is erroneous, is commented out.
; Eric Melz's newer version follows.

;(defun inhibit-multiple-mappings (list-of-objs-or-concs weight)
; (do ((lst (remove-nil-dup list-of-objs-or-concs) (cdr lst)))
; ; exit:
; ((null lst) (get *the-person* 'total-links))
; (if (atom-begins (car lst) #\?) ; query
; (excit-queries (car lst))
; ; otherwise:
; (make-inhib-links (no-queries (get (car lst) 'constraint-hyps))
; weight
; )
; )
; )
;)


; **************************
; PRAGMATICS:
; **************************

; *******************************
; CHECK-IMPORTANCE checks a structure to see if it is of the type problem,
; and, if so, makes appropriate connections to the "pragmatic" unit.
; The units connected include all hypotheses about predicates and propositions
; in the goal, and all hypotheses about propositions and predicates of
; proposition mentioned in the goal. If *propns-import?* is nil, the
; proposition connections will not be made.

;; Added # before each '(lambda ...)   -MA 10/2011
;; PROBABLY NEEDS PERSONALIZATION EG WITH PERSONAL-GET-ARGS

(defun check-importance (struc)
  (error "CHECK-IMPORTANCE is not fully implemented? See acme-personal.lisp")
  struc) ; <- stops compilers from complaining about unused lexical var

; Commented out by MA 10/2011 because mapped-concepts and mapped-propositions 
; aren't defined anywhere, and SBCL complained.  Fix this when needed.
; (defun check-importance (struc)
;   (prog (goal goalprops)
;         (if (or
;              (not (equal (get struc 'data-type) 'problem))
;              (not (member 'goal (get struc 'fields))))
;             (return nil))
;         (setf goal (get struc 'goal))
;         ; If any predicates have been mapped, IMPORTANT them.
;         (important
;          (remove-if-not #'(lambda (el) (member el mapped-concepts))
;                         (mapcar #'get-pred goal)))
;         ; If any propositions have been mapped, IMPORTANT them.
;         (if *propns-import?*
;             (important
;              (remove-if-not #'(lambda (el) (member el mapped-propositions))
;                             (mapcar #'personal-get-propn-name goal))))
;         ; If any arguments which are propositions have been mapped,
;         (setf goalprops
;               (remove-if-not #'(lambda (el) (member el mapped-propositions))
;                              (apply #'append (mapcar #'get-args goal))))
;         ; IMPORTANT their predicates,
;         (important
;          (remove-if-not #'(lambda (el) (member el mapped-concepts))
;                         (mapcar #'pred-from-propn goalprops)))
;         ; and IMPORTANT them.
;         (if *propns-import?* (important goalprops))))

; *****************************
; PRESUMED notes that certain mapping hypotheses or desired or presumed.
; ASSUMES *THE-PERSON* HAS BEEN SET CORRECTLY.
(defun presumed (list-of-units)
  (create-pragmatic-unit)
  (do ((units list-of-units (cdr units)))
    ((null units) 'done)
    ; repeat:
    (let ((personal-unit (generic-to-personal-sym (car units))))
      (unless *silent-run?* (my-print personal-unit '" is a presumed mapping."))
      (make-symlink 'pragmatic personal-unit *prag-weight*))))

(defun desired (lst) (presumed lst))

; *****************************
; IMPORTANT indicates that the system has a special interest
; in an element (concept, object, proposition) although it
; does not desire any particular map.
; (Its effect is to add a link/weight to the pragmatic unit to every
; map-node which has the element on one side or the other of the map.)
; ASSUMES *THE-PERSON* HAS BEEN SET CORRECTLY.
(defun important (list-of-els)
  (create-pragmatic-unit)
  (do ((els (mapcar #'generic-to-personal-sym list-of-els) (cdr els))) ; added g-t-p-s 12/1/11 -MA
      ((null els) 'done) ;
    (unless *silent-run?* (my-print (car els) " is an important element."))
    (make-excit-links 'pragmatic
                      (get (car els) 'constraint-hyps)
                      *import-weight*)))

; *****************************
; CREATE-PRAGMATIC-UNIT makes a special pragmatic unit if necessary.

; *THE-PERSON* MUST BE SET PROPERLY
(defun create-pragmatic-unit ()
  (cond ((null *pragmatic-unit-made*)
         (put 'pragmatic 'activation 1.0L0) ; clamp it
         (setf (get *the-person* 'all-units) (remove 'pragmatic (get *the-person* 'all-units)))
         (setf *pragmatic-unit-made* t))))

; **************************

; NO-QUERIES removes mappings about queries from a list of units.

(defun no-queries (lst)
  (do ((ls lst (cdr ls))
       (result nil))
      ((null ls) result)
    (unless (or (is-query (car (get (car ls) 'concerns)))
                (is-query (second (get (car ls) 'concerns))))
      (push (car ls) result))))



; IS-QUERY

(defun is-query (atm)
  (atom-begins atm #\?))

; *****************************
; BEST-MATCH figures out what concept or object has been found to be
; the best match.

(defun best-match (conc-or-obj)
  (let (bm best-unit other-good-units)
    (setf best-unit (highest-l (get conc-or-obj 'constraint-hyps)
                               'activation))
    (if (listp best-unit)
        (setf best-unit (car (setf other-good-units best-unit))))
    (setf bm (other-from-pair conc-or-obj (get best-unit 'concerns)))
    (my-print "Best mapping of " conc-or-obj " is " bm ". "
              (get best-unit 'activation))
    (if other-good-units
        (do ((units (cdr other-good-units) (cdr units)))
            ((null units) nil)
          (setf bm (other-from-pair conc-or-obj (get (car units) 'concerns)))
          (my-print " tied with " bm ".")))
    
    (if *show-others?*
        (good-enough-s conc-or-obj
                       (remove best-unit
                               (get conc-or-obj 'constraint-hyps))))
    (setf *best-matches* (cons (list conc-or-obj bm) *best-matches*))))

; *********************************
; OTHER-FROM-PAIR pick the other out of a pair that includes the given atom.

(defun other-from-pair (atm lst)
  (if (eq atm (car lst)) (second lst)
    (car lst)))

; **********************************
; GOOD-ENOUGH-S checks whether other mappings are over a threshold.

(defun good-enough-s (el units)
  (do ((unts units (cdr unts)))
      ((null unts) 'done)
    (good-enough el (car unts))))

; **********************************
; GOOD-ENOUGH says if a mapping has an activation over a threshold.

(defun good-enough (el unit)
  (if (> (get unit 'activation) *min-good*)
      (my-print " Mapping with "
                (other-from-pair el (get unit 'concerns))
                " is also possible: "
                (get unit 'activation))))

; ***********************************
; CATNAME is used by ACME and ARCS to put together hypothesis unit names.
; Conversion to MCL, PT, 5-95
; Added person name to distinguish nodes of different persons (Marshall Abrams, 6/2011)
; *THE-PERSON* MUST BE SET PROPERLY
(defun catname (unit1 unit2)
  (generic-to-personal-sym
    (read-from-string (coerce (append (coerce (princ-to-string unit1) 'list)
                                      (coerce "=" 'list)
                                      (coerce (princ-to-string unit2) 'list))
                              'string))))


; Next few helper functions added by Marshall Abrams 6/2011
; unit-type=acme distinguishes map units from others e.g. pure proposition units

; true if unit-type is acme
(defun is-acme-unit (unit)
  (eq 'acme (get unit 'unit-type)))

; like is-acme-unit: true if unit-type is acme. but here true value is the unit itself.
(defun acme-unit (unit)
  (if (is-acme-unit unit)
    unit
    nil))

; true if all unit-types are acme
(defun all-are-acme-units (&rest units)
  (eval (cons 'and (mapcar #'is-acme-unit units))))

; true if unit-type is not acme
(defun not-acme-unit (unit)
  (not (is-acme-unit unit)))


; CONCAT is similar to catname, but more general, used in PI.
; TODO? Should this be replaced by the CL primitive CONCATENATE? -MA
(defun concat (&rest concat-things)
  (read-from-string (coerce (apply #'append
                                   (mapcar #'atom-to-list
                                           concat-things))
                            'string)))

; ATOM-TO-LIST turns an atom into a list
; i.e. it turns it into a list of characters
(defun atom-to-list (atm)
  (coerce (princ-to-string atm) 'list))

; NOT-MEMBER ; for use with symbols produced by catname
(defun not-member (el lst)
  (cond ( (memberlist el lst) nil )
        ( t t)))

; MEMBERLIST
; Because member otherwise uses eq
(defun memberlist (el lst2)
  (member el lst2 :test 'equal))



;;; The original BEST-MATCH procedure has a bug where if the concept or object
;;; is in the target, it will also consider these mappings (i.e. from target
;;; to source rather than from source to target). This patch fixes that by
;;; adding an extra "filter" to the form (get conc-or-obj 'constraint-hyps),
;;; namely, it only retrieves units in which conc-or-obj is the first element
;;; of the mapping unit.
;;; notes:
;;; conc-or-obj is a predicate or argument symbol.
;;; units is a list of acme map units, e.g. from the 'constraint-hyps property of conc-or-obj.
;;; Note that the 'concerns property of a map unit lists the two symbols which are mapped by it.
(defun get-firsts (conc-or-obj units)
  (let ((ret nil))
    (dolist (unit units)
      (when (equal (first (get unit 'concerns)) conc-or-obj)
        (setf ret (cons unit ret))))
    ret))

;;; get-seconds is like get-firsts except it does it for the target
(defun get-seconds (conc-or-obj units)
  (let ((ret nil))
    (dolist (unit units)
      (when (equal (second (get unit 'concerns)) conc-or-obj)
        (setf ret (cons unit ret))))
    ret))
