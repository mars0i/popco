; FILE: network.lisp
; PURPOSE: network operations for COHERE
; UPDATED: 6-23-2000

; Mainly by Paul Thagard and collaborators, 
; but with many modifications by Marshall Abrams (2011, 2012, 2013)
; (Many are marked. Code tied to references to *the-person* are by MA.)

; 1/3/2012 commented out type and ftype declarations.
; In SBCL, they don't speed up, and some of them significantly slow runs.


;*********************************************************************************
;; GET/PUT MACROS

(defmacro activation (unit) `(get ,unit 'activation)) ; note ACT is a function version of this
(defmacro prop (unit) `(get ,unit 'prop))
(defmacro original-activation (unit) `(get ,unit 'original-activation))
(defmacro facilitates (unit) `(get ,unit 'facilitates))
(defmacro facilitated-by (unit) `(get ,unit 'facilitated-by))
(defmacro contradicts (unit) `(get ,unit 'contradicts))
(defmacro new-activation (unit) `(get ,unit 'new-activation))
(defmacro links-from (unit) `(get ,unit 'links-from))
(defmacro person-of (unit) `(get ,unit 'person-of))
(defmacro dont-sum-weights (unit) `(get ,unit 'dont-sum-weights)) ; added by MA 12/11
(defmacro do-sum-weights (unit) `(not (dont-sum-weights ,unit)))  ; added by MA 12/11 [This way, unmarked units in older code will sum by default]

; WEIGHT-OF-LINK-BETWEEN finds the value of a link between two units.
; Changed from function to macro 4/20/2012 -MA
; Note doesn't know the difference between symlinks and one-way links.
(defmacro weight-of-link-between (unit1 unit2)
  `(or (cdr (assoc ,unit2 (links-from ,unit1))) 0L0)) ; i.e. if unit2 is listed in unit1's list of links, return the link weight, otherwise 0

; def is in utilities-personal.lisp
;(defmacro plist (atm) `(symbol-plist ,atm))

; activation macro wrapper for mapcar, etc.:
(defun get-activation (unit) (activation unit))


; ****************************************
; * FUNCTIONS FOR CLEARING THE NETWORK *

; CLEAR-PROPS clears a unit's property list.
(defun clear-props (atm)
  (setf (symbol-plist atm) nil))

; ****************************************

; CLEAR-NET starts the beginning of a run by cleaning up the
; results of past runs.

; *THE-PERSON* MUST BE SET PROPERLY
; MANY OF THE VARIABLES BELOW MUST BE PERSONALIZED BEFORE BEING USED.
(defun clear-net ()
  (unless *silent-run?* (my-print '"Clearing net."))
  (setf *stop-run?* nil)
  ; clear all property lists:
  (clear-props 'special)
  (mapcar #'clear-props (get *the-person* 'all-units))
  (mapcar #'clear-props (get *the-person* 'all-valence-units))
  (unless *silent-run?* (my-print "Units cleared."))
  (setf (get *the-person* 'total-times) 1)
  (setf (get *the-person* 'total-links) 0)
  (setf (get *the-person* 'all-units) nil)
  (setf (get *the-person* 'all-data) nil)
  (setf (get *the-person* 'asymptoted-units) nil)
  (setf (get *the-person* 'all-propositions) nil)
  (setf *all-supergoals* nil)
  (setf *all-subgoals* nil)
  (setf *all-basic-goals* nil)
  (setf *contradictions* nil)
  (setf (get *the-person* 'settled?) nil)
  (setf (get *the-person* 'propn-net-settled?) nil)
  (setf (get *the-person* 'analogy-net-settled?) nil)
  (mapcar #'clear-struc (get *the-person* 'all-structures)) ; for ACME
  (setf (get *the-person* 'all-units) nil)
  (setf (get *the-person* 'all-valence-units) nil) ; for HOTCO
  (setf (get *the-person* 'evaluation-units) nil) ; for HOTCO 2
  (setf *all-xs* nil *all-xsit* nil) ; for expln. schemas
  (unless *silent-run?* (my-print "Network cleared.")))



; ********************************************
; * FUNCTIONS FOR CREATING UNITS AND LINKS *
; ********************************************

; NOTE-UNIT sets the global variables indicating a unit has been added.
; And sets the initial activation value.
; Added optional activation value to override default value (9/2011 MA)
; NOTE THAT this in effect creates the node if it didn't already exist.
; *THE-PERSON* MUST BE SET CORRECTLY or passed as arg.
(defun note-unit (unit &optional my-init-activn (person *the-person*))
  (let ((new-activn (or my-init-activn *init-activ*)))
    (DECLARE (LONG-FLOAT NEW-ACTIVN))
    (setf (activation unit) new-activn) ; sets activn even if nil is passed
    (setf (person-of unit) person)
    (pushnew unit (get person 'all-units))))

; Older version:
;(defun note-unit (unit)
;  (setf (activation unit) *init-activ*)
;  (pushnew unit (get *the-person* 'all-units)))

; MAKE-SYMLINKS creates symmetric links between all of the members of a set
; of units.
; *THE-PERSON* MUST BE SET CORRECTLY: See comment on make-link for explanation.
(defun make-symlinks (list-of-units weight)
  (DECLARE (LONG-FLOAT WEIGHT))
  (do ((units list-of-units (cdr units)))
      ((null units) nil)
    (dolist (unit (cdr units))
      (make-symlink (car units) unit weight))))

; MAKE-SYMLINK sets up a symmetric link between units.
; Important to make no links from unit to itself, and don't clobber
; excitatory links with inhibitory ones.  Also won't make an acme map
; unit unless it doesn't exist.
; Excitatory links sum, but inhibitory don't.
; This slows down creation of symlinks, but only has to be done once.
; *THE-PERSON* MUST BE SET CORRECTLY to update number of links in person in make-link.
; However, DOES NOT CONVERT GENERIC/PERSONAL ETC.: Uses whatever is passed in as the unit name.
; 4/20/2012 split the old OR into two cond tests.
(defun make-symlink (unit1 unit2 weight)
  (DECLARE (LONG-FLOAT WEIGHT))
  (if (not (eq unit1 unit2))                  ; if distinct nodes
    (let ((old-weight (weight-of-link-between unit1 unit2)))
      (cond ((= old-weight 0)             ; and either never-linked [MAYBE REPLACE WITH UNLINKED?]
             (raw-make-symlink unit1 unit2 weight +acme-max-weight+)) ; then call make-link to create link, or update weight if existing excitatory link with unmarked units
            ((and (> old-weight 0)        ; or [excitatorily-linked
                  (do-sum-weights unit1)  ;    and neither node marked to avoid summing link weights
                  (do-sum-weights unit2)) ;    [popco avoids summing due to conversation.]
             (raw-make-symlink unit1 unit2 weight +acme-max-weight+)))))) ; then call make-link to create link, or update weight if existing excitatory link with unmarked units

;; RAW-MAKE-SYMLINK
;; Added 4/20/2012, replaced separate make-link calls in make-symlink with this. -MA
; *THE-PERSON* MUST BE SET CORRECTLY to update number of links in person.
; And to record symlinks as new in person.
(defun raw-make-symlink (unit1 unit2 weight &optional (max-weight 1L0) (min-weight -1L0))
  (DECLARE (LONG-FLOAT WEIGHT))
  (make-link unit1 unit2 weight max-weight min-weight)
  (make-link unit2 unit1 weight max-weight min-weight))

; ADD-WEIGHT
; I doubt it matters, but I'm definining this as a macro in case it's more efficient;
; should only be used in make-link.
(defmacro add-weight (addl-weight old-weight max-weight min-weight)
    `(max ,min-weight (min ,max-weight (+ ,addl-weight ,old-weight))))

; MAKE-LINK sets up a 1-way link. It adds the weight to whatever
; weight (initially 0) was on the link.
; [Adding weights is needed e.g. for ECHO; see Conceptual Revolutions p. 99. -MA]
; *THE-PERSON* MUST BE SET CORRECTLY to update number of links in person.
; However, DOES NOT CONVERT GENERIC/PERSONAL ETC.: Uses whatever is passed in as the unit name.
(defun make-link (unit1 unit2 weight &optional (max-weight 1L0) (min-weight -1L0))
  (DECLARE (LONG-FLOAT WEIGHT))
  (let ((link (assoc unit2 (links-from unit1)))) ; check if there's already a link from unit2 to unit1; if non-nil,
    (cond (link                                  ;   this is the cons in links-from with unit2 as car and old weight as cdr
           (rplacd link (add-weight weight (cdr link) max-weight min-weight)))  ; replace cdr of old entry in unit1's links-from with added weight 
          (t                                              ;  NOTE no attempt to place a max on link weight--can even exceed 1. -MA 3/12
           (setf (links-from unit1)                       ; otherwise just create a new links-from entry
                 (acons unit2 weight (links-from unit1)))
           (setf (get *the-person* 'total-links) (1+ (get *the-person* 'total-links)))))))


; ***************************************
; * FUNCTIONS FOR RUNNING THE NETWORK *
; ***************************************

(defun settle-person-net (person units-property settled-property)
  (unless (get person settled-property)
    (cond ((or (not *min-pop-ticks-to-settle*) (<= *pop-tick* *min-pop-ticks-to-settle*))                   ; early on, we don't check for settling--just get started [CHANGE FOR BIRTH/REPRODUCTION]
           ;(format t "I'm not going to settle! (*min-pop-ticks-to-settle* = ~S)~%" *min-pop-ticks-to-settle*) ; DEBUG
           (settle-n-iters (get person units-property) *max-times*))
          (t 
            ;(format t "at least *min-pop-ticks-to-settle*. settled-property = ~S~%" (get person settled-property)) ; DEBUG
            (setf (get person settled-property)                     ; later, we check to whether the network settled, to avoid unnecessary settling
                   (settle-up-to-n-iters (get person units-property) *max-times*))))))

(defun settle-n-iters (units iters)
  (DECLARE (FIXNUM ITERS))
  ;(format t "settle-n-iters: iter = ") ; DEBUG
  (dotimes (i iters)
    (DECLARE (FIXNUM I))
    ;(format t "~S " i) ; DEBUG
    (mapc #'update-unit-activn-gross units)
    (mapc #'fix-activn units)))

; do-less tail-recursive version seems clearer than do loop version below
(defun settle-up-to-n-iters (units remaining-iters)
  (DECLARE (FIXNUM REMAINING-ITERS))
  ;(format t "settle-up-to-n-iters: remaining-iters = ~S~%" remaining-iters) ; DEBUG
  (mapc #'update-unit-activn-gross units)
  (let ((settled (every #'identity (mapcar #'fix-activn units))))  ; EVERY #'IDENTITY means "and". [Can't apply AND; it's a macro.]
    (if (or settled (= remaining-iters 1))
      settled
      (settle-up-to-n-iters units (1- remaining-iters)))))

;; FIX-ACTIVN
;; Shifts new-activation into activation, now that we've updated all of the new-activations
;; based on what was in the old activations.  Also tests whether activation has stopped
;; changing and returns t if it has, nil otherwise.
;; NOTE no longer recording asymptoted units.
;; [Replacement for fix-activation, formerly called by run-hyp-net 11/2012]
(defun fix-activn (unit)
  (let ((old-activn (activation unit))
        (new-activn (new-activation unit)))
    (DECLARE (LONG-FLOAT OLD-ACTIVN NEW-ACTIVN))
    (let ((asymptoted (< (abs (- new-activn old-activn))
                         *asymptote*)))
      (setf (activation unit) new-activn)
      asymptoted)))


; MCLELLAN/RUMELHART SETTLING FUNCTION. NOT CURRENTLY USED BY POPCO, BUT WORTH KEEPING AROUND
; UPDATE-UNIT-ACTIVN updates the activation of a unit based on the
; links it has.
;(defun update-unit-activn (unit)
;  (declare (ftype (function (&rest float) float) min max + * -)
;           (ftype (function (float float) symbol) >))
;  (let ((net-input-value (net-input unit)))
;    (declare (type (float) net-input-value))
;    (setf (new-activation unit)
;          (min *max-activation*
;               (max *min-activation*
;                    (+ (* (activation unit) (- 1.0 *decay-amount*))
;                       (if (> net-input-value 0.0)
;                           (* net-input-value
;                              (- *max-activation* (activation unit))
;                              )
;                         ; else:
;                         (* net-input-value
;                            (- (activation unit) *min-activation*)))))))))
; NET-INPUT is the weighted sum of output from all input units.
;(defun net-input (unit)
;  (declare (ftype (function (&rest float) float) max + *))
;  (do ((links (links-from unit) (cdr links))
;       (result 0.0))
;      ((null links) result)
;    (declare (type (float) result))
;    (setf result (+ (* (float (cdar links))
;                       (max *output-threshold* (activation (caar links))))
;                    result))))


; ANNOUNCE-ASYMPTOTE informs the user of any units recently asymptoted.
(defun announce-asymptote (unit)
  (my-print "Unit " unit " reached asymptote at cycle "
            (get *the-person* 'total-times) " with activation "
            (new-activation unit) "."))

; UPDATE-UNIT-ACTIVN-GROSS updates the activation of a unit based on the
; links it has, using Grossberg's rule that treats excitation and
; inhibition separately.
; Uses global variables *current-excit* and *current-inhib*
(defun update-unit-activn-gross (unit)
  ;(declare (ftype (function (&rest float) float) + - * min max))
  ; calculate excitation and inhibition.
  (if *tversky?*
    (excit-and-inhib-tversky unit)
    (excit-and-inhib unit))
  (let ((activn (THE LONG-FLOAT (activation unit))))
    (DECLARE (LONG-FLOAT ACTIVN))
    (setf (new-activation unit)
          (min *max-activation*
               (max *min-activation*
                    (+ (* activn (- 1.0L0 *decay-amount*))
                       (* (- *max-activation* activn) *current-excit*) ; *current-excit* and *current-inhib* are set by excit-and-inhib*
                       (* (- activn *min-activation*) *current-inhib*))))))) ; and contain sums of activns from linked nodes

; EXCIT-AND-INHIB is just like net-input, except that it keeps track
; of excitation and inhibition separately. EXCIT-AND-INHIB-TVERSKY is
; identical except that units with negative activation can pull down
; their excitatory neighbors.
; The algorithm is roughly: sum the positive and negative activations of
; linked nodes, separately, and then set these into two global variables,
; which are used to pass these sums to e.g. update-unit-activn-gross.
(defun excit-and-inhib (unit)
  ;(declare (ftype (function (&rest float) float) max + *)
  ;         (ftype (function (float float) symbol) >))
  (do ((excit 0.0L0) (inhib 0.0L0) (wt 0.0L0) (activn 0.0L0)
       (links (links-from unit) (cdr links)))
      ((null links)
       (setf *current-excit* excit)
       (setf *current-inhib* inhib))
    (DECLARE (LONG-FLOAT EXCIT INHIB WT ACTIVN))
    (setf wt (cdar links))
    (setf activn (max *output-threshold* (THE LONG-FLOAT (activation (caar links))))) ; *output-threshold* is usually 0.
    (if (> wt 0.0L0)
      (setf excit (+ excit (* wt activn)))
      ; else wt is inhibitory:
      (setf inhib (+ inhib (* wt activn))))))

(defun excit-and-inhib-tversky (unit)
  ;(declare (ftype (function (&rest float) float) max + *)
  ;         (ftype (function (float float) symbol) >))
  (do ((excit 0.0) (inhib 0.0) (wt 0.0)
       (links (links-from unit) (cdr links)))
      ((null links)
       (setf *current-excit* excit)
       (setf *current-inhib* inhib))
    ;(declare (type (float) excit inhib wt))
    (setf wt (float (cdar links)))
    (if (> wt 0.0)
        (setf excit (+ excit (* wt (activation (caar links)))))
      ; else wt is inhibitory:
      (setf inhib (+ inhib (* wt (max *output-threshold*
                                      (activation (caar links)))))))))


; these will be resolved during compilation

(defun devowel (name)
  (coerce
   (do ((tname (coerce (princ-to-string name) 'list) tname)
        (vowels '(#\A #\E #\I #\O #\U) (cdr vowels))
        (cvowel '#\A (car vowels))
        )
       ((null cvowel) tname)
     (setf tname (remove cvowel tname :test #'equal))
     )
   'string
   )
  )


(defun rep (value count)
  "Make a list consisting of COUNT elements of VALUE."
  (make-sequence 'list count :initial-element value))


; FUNCTIONS ADDED BY MARSHALL ABRAMS:

; added by MA 11/2011
;(defun set-link-weight (unit-to-set other-unit weight)
;  (let ((link (assoc other-unit (links-from unit-to-set))))
;    (when link (setf (cdr link) weight))))

; Added by MA 12/11
(defun show-symlink (unit1 unit2)
  (list (assoc unit2 (links-from unit1))
        (assoc unit1 (links-from unit2))))

; SET-SYMLINK-WEIGHT: Set weight of an existing symlink. Added by MA 11/2011.
(defun set-symlink-weight (unit1 unit2 weight)
  (DECLARE (LONG-FLOAT WEIGHT))
  (let ((unit1-link (assoc unit2 (links-from unit1)))
        (unit2-link (assoc unit1 (links-from unit2))))
    (if (not (and unit1-link unit2-link))
      (error "set-symlink-weight: Can't set symlink weight to ~S. No two-way link between ~S and ~S."
             weight unit1 unit2))
    (setf (cdr unit1-link) weight)
    (setf (cdr unit2-link) weight)))

; Convenience function to set sym link weight from the representation
; produced by list-constraints and sometimes stored in person 
; property all-constraints.  Added by MA 12/11
(defun set-symlink-weight-from-constraint (constraint)
  (set-symlink-weight (car constraint) (cadr constraint) (THE LONG-FLOAT (cddr constraint))))
; tip - example:
;(mapc #'set-symlink-weight-from-constraint (mapcar #'maybe-personalize-constraint list-of-constraints))

; Added by MA 11/11
(defun symlinked? (unit1 unit2)
  (and (assoc unit2 (links-from unit1))
       (assoc unit1 (links-from unit2))))

; Note unlinked? is not equivalent to not symlinked? if assymetric links are in use.
(defun unlinked? (unit1 unit2)
  (not (or (assoc unit2 (links-from unit1))
           (assoc unit1 (links-from unit2)))))

; Old COHERE-style test for being unlinked.  
; Fails on POPCO's zero-weight proposition/salient links.
(defun cohere-style-unlinked? (unit1 unit2)
  (= 0.0L0 (THE LONG-FLOAT (weight-of-link-between unit1 unit2))))

(defun abs-activn-change (unit)
  (abs 
    (- (THE LONG-FLOAT (get unit 'activation))
       (THE LONG-FLOAT (get unit 'new-activation)))))

;; Sum of differences between new-activation and activation in units.
(defun sum-activn-changes (units)
  (apply #'+ (mapcar #'abs-activn-change units)))

;; Average of differences between new-activation and activation in units.
(defun avg-activn-change (units)
  (/ (sum-activn-changes units) (length units)))

; check whether an element in list from a links-from property is a link-from unit
(defun is-link-from (unit link)
  (eq unit (car link)))

(defun unlink-units (unit1 unit2)
  (setf (links-from unit1) 
        (remove-if #'(lambda (link) (is-link-from unit2 link))
                   (links-from unit1)))
  (setf (links-from unit2) 
        (remove-if #'(lambda (link) (is-link-from unit1 link))
                   (links-from unit2))))

(defun unlink-all-from (unit)
  (mapc #'(lambda (link) (unlink-link link unit))
        (get unit 'links-from)))

; helper function
(defun unlink-link (link unit)
  (unlink-units (car link) unit))
