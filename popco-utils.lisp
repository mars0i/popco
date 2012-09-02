;; popco-utils.lisp
;; Copyright (c) 2011 by Marshall Abrams
;; May be distributed only with permission from the author.

; tips:
; (setf oldstate (copy-symbol personetc t))
; (tree-equal (symbol-plist oldstate) (symbol-plist personetc))
; note though that this shares units (but not lists) with the old personetc.
; if you didn't copy-symbol the units, their activation will change in
; lockstep, since identical.
; use REMOVE to filter a list

; print to the console a person's constraints in space-delimited format
(defun print-constraints-example1 (person)
  (mapc 
    #'(lambda (c) (format t "~S ~S ~S~%" (car c) (cadr c) (cddr c))) 
    (mapcar #'(lambda (constraint) `(,(personal-to-generic-sym (car constraint)) 
                                      ,(personal-to-generic-sym (cadr constraint)) . ,(cddr constraint))) 
            (list-constraints (get person 'all-units)))) 
  t)

; print to the console a person's constraints in space-delimited format
(defun print-constraints-example2 (person)
  (setf *the-person* person)
  (mapc 
    #'(lambda (c) (format t "~S ~S ~S~%" 
                          (personal-to-generic-sym (car c))  
                          (personal-to-generic-sym (cadr c)) 
                          (cddr c)))
    (list-constraints (get person 'all-units))) 
  t)

; print to the console a person's constraints in csv format
(defun print-constraints-csv (person)
  (setf *the-person* person)
  (mapc 
    #'(lambda (c) (format t "~S,~S,~S~%" 
                          (personal-to-generic-sym (car c))  
                          (personal-to-generic-sym (cadr c)) 
                          (cddr c)))
    (list-constraints (get person 'all-units)))
  t)

; print to the console a person's constraints in csv sorted format
(defun sort-print-constraints-csv (person)
  (setf *the-person* person)
  (mapc 
    #'(lambda (c)
        (let ((a (personal-to-generic-sym (car c)))  
              (b (personal-to-generic-sym (cadr c))))
             (format t "~S,~S,~S~%" (if (string< a b) a b) (if (string< a b) b a) (cddr c))))
    (list-constraints (get person 'all-units)))
  t)


;;;doesn't work yet... trying to write constraints to a file, or just return them
#|  (defun write-constraints-csv (person )
  (setf *the-person* person)
  ((mapc 
    #'(lambda (c) (format nil "~S,~S,~S~%" 
                          (personal-to-generic-sym (car c))  
                          (personal-to-generic-sym (cadr c)) 
                          (cddr c)))
    (list-constraints (get person 'all-units)))
      t))  |#

; list the property names of a symbol
(defun sym-props (sym)
  (sym-props-aux (plist sym)))

(defun sym-props-aux (pl)
  (if (null pl)
    ()
    (cons (car pl) (sym-props-aux (cddr pl)))))

;; RANDOM-SUBSET
;; Does what you think it does: Give it a length and a list, and it will give
;; you a new list of that length randomly chosen from the list.
;; [Honestly, tried to write this as a loop, but it wasn't working right, and the
;; tail-recursive version worked the first time.]
(defun random-subset-aux (remaining oldset newset)
  (if (or (<= remaining 0) 
          (null oldset))
    newset
    (let ((elt-to-move (elt oldset (random (length oldset)))))
      (random-subset-aux (1- remaining)
               (remove elt-to-move oldset)
               (cons elt-to-move newset)))))

(defun random-subset (size superset)
  (random-subset-aux size superset ()))


;; SAFE SORT FUNCTIONS
;; The built in Lisp functions sort and stable-sort are destructive--they can modify
;; the list which is their argument, causing surprises--and normally should only be used on a
;; copy of a list.  Here are convenience functions that do this:

(defun sort-copy (lis fn)
  (sort (copy-list lis) fn))

(defun stable-sort-copy (lis fn)
  (stable-sort (copy-list lis) fn))


;; SPECIAL MAP FUNCTIONS:

;; MAPCAR-WITH-TAIL
;; Works just like mapcar for a single list, but can do something different to the
;; last element of the list.  Calls fn on each element of the list except the last,
;; on which tail-fn is called.
(defun mapcar-with-tail (fn tail-fn lis)
  (cond ((null lis) ())    ; only invoked when passed an empty list from the start
        ((null (cdr lis))  ; look ahead to see if this is the last element
         (list (funcall tail-fn (car lis)))) ; if so then call tail-fn rather than fn.
        (t 
          (cons (funcall fn (car lis))
                (mapcar-with-tail fn tail-fn (cdr lis))))))

;; CROSS-MAPCAR
;; applies a 2-argument fn to the cross product of lis1 and lis2, i.e. to all possible pairs, one element from each list
(defun cross-mapcar (fn lis1 lis2)
  (apply #'append
         (mapcar #'(lambda (x2)
                     (mapcar #'(lambda (x1) (funcall fn x1 x2))
                             lis1))
                 lis2)))


(defun symbol-lessp (sym1 sym2)
  (string-lessp (symbol-name sym1) (symbol-name sym2)))

(defun symbol< (sym1 sym2)
  (string< (symbol-name sym1) (symbol-name sym2)))

(defun symbol-sort (syms)
  (stable-sort (copy-list syms) #'symbol<))

; REAL-TIME-ELAPSED-SINCE
; Return number of seconds elapsed since earlier time as a float.
(defun real-time-elapsed-since (earlier)
  (coerce (/ (- (get-internal-real-time) earlier) 
             internal-time-units-per-second)
          'float))

; RUN-TIME-ELAPSED-SINCE
; Return number of seconds of "run time" elapsed since earlier time as a float.
; The CL Spec says:
;	The precise meaning of this quantity is implementation-defined; it may
;	measure real time, run time, CPU cycles, or some other quantity. The
;	intent is that the difference between the values of two calls to this
;	function be the amount of time between the two calls during which
;	computational effort was expended on behalf of the executing program.
(defun run-time-elapsed-since (earlier)
  (coerce (/ (- (get-internal-run-time) earlier) 
             internal-time-units-per-second)
          'float))


; Returns first n persons in the population's list of members:
(defun first-n-persons (n)
  (subseq (get *the-population* 'members) 0 n))

; Returns last n persons in the population's list of members:
(defun last-n-persons (n)
  (let* ((members (get *the-population* 'members))
         (pop-size (length members)))
    (subseq members (- pop-size n) pop-size)))

; Returns persons n through m in the population's list of members:
; Indexes are ONE-BASED, so e.g. if the list is of length len, use args 1, len to get the whole list.
(defun persons-n-thru-m (n m)
  (subseq (get *the-population* 'members) (1- n) m))

; REM-ELT-FROM-PROPTY 
; if contents of sym's property propty is a list or other sequence, alters
; the property contents by deleting element elt from that list/sequence.
(defun rem-elt-from-property (elt sym propty)
  (setf (get sym propty) 
        (remove elt (get sym propty))))

; SIGN-OF
; Return 1 or -1 depending on whether > or < 0.
; Note returns 1 for zero.
(defun sign-of (num)
  (if (minusp num) -1 1))

; List the keys (indicators) which are actually listed in the property list of a symbol.
; Note that keys which have never been created still "exist", but have value nil.
(defun plist-keys (sym)
  (keys-from-plist (symbol-plist sym)))

; Note this won't notice if a property list is malformed by having an odd number of elements: cdr of nil is nil.
(defun keys-from-plist (plist)
  (if (null plist)
    nil
    (cons (car plist) 
          (keys-from-plist (cddr plist)))))

(defun activn-greaterp (u1 u2)
  (> (activation u1) (activation u2)))

; Reproduce structure of a two trees, with t's where
; they match, and left as is where they differ and below.
(defun tree-diff (branch1 branch2)
    (cond ((and (atom branch1) (atom branch2)) ; if both atoms
           (if (eq branch1 branch2)            ; and same
             t                                 ; return t
             (cons branch1 branch2)))          ; otherwise, return offending atoms
          ((or (atom branch1) (atom branch2))  ; if only one is atom, that's a diff
           (cons branch1 branch2))             ; so return offending branches
          (t                                   ; otherwise recurse
            (cons (tree-diff (car branch1) (car branch2))
                  (tree-diff (cdr branch1) (cdr branch2))))))

(defun maptree (fn tree)
  (cond ((null tree) '())
        ((atom tree) (funcall fn tree))
        (t (cons (maptree fn (car tree))
                 (maptree fn (cdr tree))))))

; Supercedes version in utilities-personal.lisp.
; I think this may be slightly faster than Thagard's in utilities.lisp.
(defun flatten (tree)
  (if (atom tree)
    tree
    (flatten-aux tree '())))

;; there's a lot of appending here--could be more efficient?
(defun flatten-aux (tree acc)
  (cond ((null tree) acc)
        ((atom tree) (cons tree acc))
        (t (append (flatten-aux (car tree) acc) 
                   (flatten-aux (cdr tree) acc)))))

; MAPCAR-WHEN
; Like mapcar, but if the result of the function application to an
; element is nil, that result--nil--is not included in the output.
; It's skipped over, creating a result list that's shorter than the
; second-argument list.
; Unlike mapcar, doesn't handle multi-argument functions and 
; multiple list arguments.
; [note that passing #'identity as the first argument will compress
; nils out of a list, like sbcl's built-in compress.]
(defun mapcar-when (fn lis)
  (if (null lis)
    lis
    (let ((result (funcall fn (car lis))))
      (if result
        (cons result (mapcar-when fn (cdr lis)))
        (mapcar-when fn (cdr lis))))))

; Given a list, create a list of all pairs of elements... unordered,
; in that the same pair in two different orders are considered duplicates,
; but on the other hand, given two lists of same elements in same
; order, will create EQUAL lists of pairs.
;(defun stable-unordered-pairs (lis)
;  (error "stable-unordered-pairs not implemented yet."))

; catname in acme-personal.lisp has more complex function -MA 7/2011
; [using this now for make-names called by n-persons in consensus-personal.lisp]
;(defun simple-catname (front back)
;  (read-from-string 
;    (concatenate 'string 
;                 (symbol-name front) 
;                 (symbol-name back))))

; old version based on PT's code [advantage over MA's simple version: front and back can be anything e.g. a number]
;(defun simple-catname (front back)
;    (read-from-string (coerce (append (coerce (princ-to-string front) 'list)
;                                      (coerce (princ-to-string back) 'list))
;                              'string)))

; revised version of PT-based version (q.v.); takes any number of arguments
(defun simple-catname (&rest things)
    (read-from-string (coerce (apply #'append 
                                     (mapcar #'name-to-charlist things))
                              'string)))

(defun name-to-charlist (thing)
  (coerce (princ-to-string thing) 'list))


; Does this symbol represent a proposition?
; Only (generic or personal) propositions have messages.
(defun propn? (sym)
  (get sym 'message)) ; nil if not proposition

; is this a node that represents a mapping between propositions?
; MAYBE THIS SHOULD BE DONE WITH A PERMANENT FLAG--WOULD BE FASTER
(defun propn-map? (sym)
  (let ((concerns (get sym 'concerns)))
    (if concerns                          ; if not rets nil
      (and
        (propn? (first concerns))         ; in theory, OR should be enough--there should never be a cross-category map
        (propn? (second concerns))))))

(defun propn-map-unit-competitors (unit)
  (mapcar #'car 
          (remove-if-not #'propn-map-competitor (get unit 'links-from))))

; Looks at an element in links-from, and tests whether it seems 
; to be a link from a competing proposition map unit.
(defun propn-map-competitor (link)
  (propn-map? (car link)))

;; Returns a list containing data on activations of all 
;; proposition map nodes in the population. [suitable for pprinting]
;; Currently not sorted within persons.
(defun pop-propn-maps (population)
  (mapcar 
    (lambda (p) 
      (list p (person-propn-maps p)))
    (get population 'members)))

(defun person-propn-maps (p)
  (mapcar 
    (lambda (u) (cons u (get-activation u))) 
    (sort 
      (remove-if-not #'propn-map? 
                     (get p 'all-units))
      #'symbol-lessp)))

; Kind of like CL's member function, but also considers the cdr
; of a dotted list to be a possible member.  [The usual member
; function errors if it gets as far as a dotted cdr.]
; [Note that nil is a dotted-member of every non-dotted list,
; but is only a member of a list if it appears explicitly as
; an element.]
; DOESN'T WORK RIGHT FOR NON-ATOM ELEMENTS EG LISTS
(defun dot-member (item lis &key test)
  (let ((tst (or test #'eq)))
    (if (atom lis)
      (funcall tst item lis)
      (or (funcall tst item (car lis))
          (dot-member item (cdr lis))))))

; Test for EQUALity of constraints, ignoring order in which nodes are listed.
; Note does require that the link weights be equal, not different numbers, 
; and not not merely equalp.  Constraints are assumed to have the format:
; (node1 node2 . weight)
; This can be used with (find ... :test #'equal-constraint) to search for
; constraints in a list.
; NOTE: this expects constraints to be in the format normally stored in person
; property 'all-constraints when this field is not empty, i.e. what the 
; functions LIST-CONSTRAINTS and RECORD-PERSON-CONSTRAINT create. 
; Listings of constraints in this format are not created by default.
(defun equal-constraint (c1 c2)
  (or (equal c1 c2)
      (equal c1 (cons (second c2)           ; reverse order of nodes
                      (cons (first c2)      ;  in second argument passed
                            (cddr c2))))))

; Test whether two constraints have the same nodes at their ends,
; ignoring the link weight.  (Normally different constraints
; with same nodes but different weights should not exist.)
; This can be used with (find ... :test #'eq-constraint-ends) to search for
; constraints in a list.
; NOTE: this expects constraints to be in the format normally stored in person
; property 'all-constraints when this field is not empty, i.e. what the 
; functions LIST-CONSTRAINTS and RECORD-PERSON-CONSTRAINT create. 
; Listings of constraints in this format are not created by default.
(defun eq-constraint-ends (c1 c2)
  (let ((c1-first (first c1))
        (c1-second (second c1))
        (c2-first (first c2))
        (c2-second (second c2)))
    (or (and (eq c1-first c2-first) (eq c1-second c2-second))
        (and (eq c1-first c2-second) (eq c1-second c2-first)))))

; CONSTRAINT-HAS-END
; Analogy constraint specifications here have the structure (unit1 unit2 . weight).
(defun constraint-has-end (unit constraint)
  (or (eq unit (first constraint))
      (eq unit (second constraint))))

; SEMANTIC-IFF-HAS-END
; Semantic-iff's have the structure (unit1 unit2 weight)
; Return true if one end of the sem-iff is unit, false otherwise.
; Specific whether the unit was the first ("zeroth") or second ("first") by using 0 or 1,
; respectively, as true.
(defun semantic-iff-has-end (unit semantic-iff)
  (or (eq unit (first semantic-iff))
      (eq unit (second semantic-iff))))

; FIND-SEMANTIC-IFFS
; order of arguments designed to allow later extension with searching also by optional second end unit
(defun find-semantic-iffs (semantic-iffs unit1)
  (remove-if-not #'(lambda (sem-iff) (semantic-iff-has-end unit1 sem-iff)) 
                 semantic-iffs))

; GET-OTHER-END
; Given a unit and a constraint or semantic-iff specification, returns
; the end that's not unit, or nil if neither end is unit.
(defun get-other-end (unit at-least-pair)
  (cond ((eq unit (first at-least-pair))
         (second (at-least-pair)))
        ((eq unit (second at-least-pair))
         (first (at-least-pair)))
        (t nil)))

(defun constraint-has-special (constraint)
  (constraint-has-end 'special constraint))

(defun constraint-has-salient (constraint)
  (constraint-has-end 'salient constraint))

; In popco, if one end is a proposition, then the other is too, unless it's a special node.
; Doesn't hurt to test both ends.
(defun constraint-has-propn (constraint)
  (or (propn? (first constraint))
      (propn? (second constraint))))

(defun symbol-properties (sym)
  (symbol-properties-aux (symbol-plist sym)))

(defun symbol-properties-aux (plist)
  (if (null plist) 
    nil
    (cons (car plist) 
          (symbol-properties-aux (cddr plist)))))

; list all members of lis which are same as elem
(defun all-members (elem lis &key (test #'equalp))
  (cond ((null lis) lis)
        ((funcall test elem (car lis))
         (cons (car lis) (all-members elem (cdr lis))))
        (t (all-members elem (cdr lis)))))

; add prefix before "-" to the last element of a proposition message
(defun add-prefix-to-msg-name (msg prefix)
  (append (butlast msg) (list (intern (format nil "~A-~A" prefix (car (last msg)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low-level tools useful in POPCO/COHERE

; Proposition message lists have, minimally three elements:
; predicate, argument list, and proposition name.
; However, some applications add one or more elements between
; the last two.  This removes them if they exist.
(defun remove-non-propn-elts (propn-lis)
  (list (first propn-lis)
        (second propn-lis)
        (car (last propn-lis))))

; These descend into a tree, converting each symbol to/from
; the personalized version.  [maptree from popco-utils.lisp]
(defun personalize-tree (list)
  (maptree #'generic-to-personal-sym list))
(defun depersonalize-tree (list)
  (maptree #'personal-to-generic-sym list))

; Convert a constraint into its generic analog if it's a personal symbol;
; otherwise return unchanged.  Note that at present, depersonalize-tree 
; won't work because the cddr of a constraint is a number rather than a symbol.
; *THE-PERSON* must be set correctly if the constraint is personal, unless a person is passed.
(defun maybe-depersonalize-constraint (constraint &optional (person *the-person*))
  (if (generic-sym? (car constraint)) ; assume no interpersonal constraints, which would be pathological
    constraint
    (cons 
      (personal-to-generic-sym (car constraint) person)
      (cons
        (personal-to-generic-sym (cadr constraint) person)
        (cddr constraint)))))

; opposite of the preceding
(defun maybe-personalize-constraint (constraint &optional (person *the-person*))
  (if (personal-sym? (car constraint)) ; assume no interpersonal constraints, which would be pathological
    constraint
    (cons 
      (generic-to-personal-sym (car constraint) person)
      (cons
        (generic-to-personal-sym (cadr constraint) person)
        (cddr constraint)))))

(defun maybe-depersonalize-sym (sym &optional (person *the-person*))
  (if (generic-sym? sym)
    sym
    (personal-to-generic-sym sym person)))

(defun maybe-personalize-sym (sym &optional (person *the-person*))
  (if (personal-sym? sym)
    sym
    (generic-to-personal-sym sym person)))

;; blow away everything in the property list of symbols
(defun clear-plists (&rest symbols)
  (mapc #'clear-props symbols)) ; network.lisp

;; clear out the constraint networks
(defun clear-person-net (person)
  (setf *the-person* person)
  (clear-net)) ; network.lisp

(defun clear-person-nets (&rest populations)
  (mapc #'(lambda (population)
              (mapcar #'clear-person-net (get population 'members)))
          populations))

; btw use REMOVE to filter a list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful stuff for experimentation.
;; Not necess. designed to embed in the code, which might interfere with transparency.
;; And copying and sorting is potentially expensive.

(defun print-person-units-activn-sorted (person &key decimal-places depersonalize) 
  (setf *the-person* person)
  (print-units-activn-sorted (get person 'all-units)
                             :decimal-places decimal-places 
                             :depersonalize depersonalize))

(defun print-person-units-alpha-sorted (person &key decimal-places depersonalize) 
  (setf *the-person* person)
  (print-units-alpha-sorted (get person 'all-units)
                             :decimal-places decimal-places 
                             :depersonalize depersonalize))

(defun report-person (person)
  (format t "~%~%Network for ~S settled by cycle ~S." person (get person 'total-times))
  (format t "~%Mappings for ~S:~%" person)
  (print-person-units-alpha-sorted person :decimal-places 2 :depersonalize t))

(defun report-person-full-floats (person)
  (format t "~%~%Network for ~S settled by cycle ~S." person (get person 'total-times))
  (format t "~%Mappings for ~S:~%" person)
  (print-person-units-alpha-sorted person :decimal-places nil :depersonalize t))

; PRINT-UNITS-ACTIVN-SORTED
; Print units sort by activation value, from highest to lowest.
; See print-units for doc on optional arg.
; NOTE :depersonalize requires *THE-PERSON* to be set correctly.
(defun print-units-activn-sorted (units &key decimal-places depersonalize) 
  (print-units
    (stable-sort (copy-list units) #'activn-greaterp)
    :decimal-places decimal-places
    :depersonalize depersonalize))

; PRINT-UNITS-ALPHA-SORTED
; Print units sort alphabetically.
; See print-units for doc on optional arg.
; NOTE :depersonalize requires *THE-PERSON* to be set correctly.
(defun print-units-alpha-sorted (units &key decimal-places depersonalize) 
  (print-units
    (stable-sort (copy-list units) #'symbol-lessp) ; symbol-lessp defined elsewhere in this file
    :decimal-places decimal-places
    :depersonalize depersonalize))

; PRINT-UNITS
; Print units in whatever order they're given.
; First keyword argument specifies number of decimal places; if missing, print the entire float.
; Second keyword argument: If non-nil, unit name will be depersonalized before displayed
;  [in which case *THE-PERSON* must be set correctly].
(defun print-units (units &key decimal-places depersonalize)
  (mapc #'print-unit units)
  t) ; don't confuse display by returning units

(defun print-unit (unit &key decimal-places depersonalize)
  (let ((displayed-unit (if depersonalize (personal-to-generic-sym unit) unit)))
    (if decimal-places
      (format t "~,vf	~S~%" decimal-places (activation unit) displayed-unit)
      (format t "~f	~S~%" (activation unit) displayed-unit))))

; PRINT-STRUC-PROPNS
; Display propositions and their activations, sorted from highest to lowest activation.
(defun print-struc-propns (struc)
  (print-units-activn-sorted (get struc 'propositions)))


; FIND-CONSTRAINTS
; Looks for elements of first list arg in second list arg, testing
; for "equality" using eq-constraint-ends, after depersonalizing the
; constraint if necessary.  i.e. finds constraints 
; linking the same nodes (possibly in different persons), 
; whether the weight is same or not.
; Returns results in a list of lists, each of which has the constraint
; to match followed by one or more matching constraints from the second arg.
; The two keyword arguments specify a person to use for depersonalizing constraints
; on each of the two lists of constraints.  If unspecified, *the-person* is used by default.
; If constraints are generic rather than personal, these arguments are ignored.
; NOTE: This is a rather expensive function with somewhat messy semantics.
; Use for experimentation, not for production code.
(defun find-constraints (constraints-to-find constraints-to-find-in &key (to-find-person *the-person*) (find-in-person *the-person*))
  (mapcar 
    #'(lambda (constr-to-find) 
        (cons constr-to-find
              (remove-if-not 
                #'(lambda (constr) 
                    (eq-constraint-ends (maybe-depersonalize-constraint constr-to-find to-find-person)
                                        (maybe-depersonalize-constraint constr find-in-person))) 
                constraints-to-find-in))) constraints-to-find))

(defun find-constraints-with-ends (units-to-find constraints-to-find-in &key (to-find-person *the-person*) (find-in-person *the-person*))
  (mapcar 
    #'(lambda (unit) 
        (cons unit
              (remove-if-not 
                #'(lambda (constr) 
                    (constraint-has-end (maybe-depersonalize-sym unit to-find-person)
                                        (maybe-depersonalize-constraint constr find-in-person))) 
                constraints-to-find-in))) units-to-find))


; RECORD-PERSON-CONSTRAINTS
; Sometimes useful to have list of all constraints stored with each
; person.  Thagard's original code stored this in the propery
; all-constraints, but we don't need to maintain it on every iteration.
; It's convenient for interactive investigation, though, and for producing graphs.
; Thagard's list-constraint runs through a list of units
; constructing representations of (symmetric) constraints 
; of the form (unit1 unit2 . weight).  We store this in the
; person's all-constraints property.
; By default uses all of a person's units, i.e. contents of property all-units.
; The optional argument allows programmer to pass some subset of all-units.
; They should be units from the same person, but the function doesn't test that.
(defun record-person-constraints (person &optional (units (get person 'all-units)))
    (setf (get person 'all-constraints) (list-constraints units)))

; RECORD-PERSON-PROPN-CONSTRAINTS
; Same thing as for all-constraints, but for all-propn-constraints, and only from
; propositions.  This could produce duplicates of data already in all-constraints,
; but seems easier to recreated it rather than filtering what's in all-constraints
; (as long as you don't do it too often).
;(defun record-person-propn-constraints (person &optional (units (get person 'all-propositions)))
;    (setf (get person 'all-propn-constraints) (list-constraints units)))

; Run SPEC-INFER on all propositions in a source analog structure, 
; reporting to stdout as we go along.
; *THE-PERSON* probably has to be set correctly.
;(defun spec-infer-all (src targ)
;  (mapc 
;    (lambda (p) 
;      (format t "~%~S:~%" p)
;      (spec-infer src
;                  targ
;                  (car (get p 'message))    ; predicate
;                  (cadr (get p 'message)))) ; objects
;    (get 'cal_source1 'propositions)))


(defun print-person-input-property (person)
  (pprint (get person 'input)) 
  (terpri))

(defun all-constraints (person)
  (get person 'all-constraints))

(defun pop-all-constraints (population)
  (pprint (mapcar #'all-constraints 
                  (reverse (get population 'members)))))

(defun all-units (person)
  (get person 'all-units))

(defun pop-all-units (population)
  (pprint (mapcar #'all-units 
                  (reverse (get population 'members)))))

; compare print-propns in utilities-personal.lisp
(defun all-propositions (person)
  (list person (get person 'all-propositions)))

(defun pop-all-propositions (population)
  (pprint (mapcar #'all-propositions 
                  (reverse (get population 'members)))))

(defun show-links (person)
  (mapcar (lambda (u) 
            (princ u) 
	    (princ ": ") 
	    (princ (get u 'links-from)) 
	    (terpri))
          (get person 'all-units)))

(defun list-activation (unit)
  (cons unit (get-activation unit)))

; compare print-propns in utilities-personal.lisp
(defun all-activations (person)
  (mapcar #'list-activation
          (get person 'all-units)))

(defun propns-of-source-strucs (person)
  (apply #'append 
         (mapcar #'propns-of-struc
                 (source-analog-strucs-of-person person))))

(defun negative-links-from (unit)
  (remove-if-not
    #'(lambda (lnk) (< (cdr lnk) 0)) 
    (get unit 'links-from)))

(defun units-negatively-linked-from (unit)
  (mapcar #'car (negative-links-from unit)))

(defun show-competitors-of-unit (unit)
  (format t "~%: ")
  (print-unit unit)
  (print-units-activn-sorted (cons unit (units-negatively-linked-from unit)))
  t)

(defun show-competitors-of-propn-map-units-of (person)
  (mapc #'show-competitors-of-unit (get person 'propn-map-units))
  t)

; Return all possible ordered pairs using one element from list1 and one element from list2.
; If you want to pair elements from within a single list, repeat it.
; Not efficient.
(defun all-pairs (list1 list2)
  (apply #'append
         (append
           (mapcar #'(lambda (e1) 
                       (mapcar #'(lambda (e2) (list E1 E2))
                               list2))
                   list1)
           (mapcar #'(lambda (e1) 
                       (mapcar #'(lambda (e2) (list E2 E1))
                               list2))
                   list1))))


; for testing
; run deep-type-compatible on all possible pairs of propns from two lists of messages
(defun check-propns (msgs1 msgs2)
  (mapc 
    #'(lambda (L) (print (list L (deep-type-compatible (args-from-propn (car l)) 
                                                       (args-from-propn (cadr l)))))) 
    (all-pairs
      (mapcar #'get-propn-name msgs1)
      (mapcar #'get-propn-name msgs2)))
  t)
