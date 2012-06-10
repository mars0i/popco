; FILE: network.lisp
; PURPOSE: network operations for COHERE
; UPDATED: 6-23-2000

; Mainly by Paul Thagard and collaborators, 
; but with many modifications by Marshall Abrams (2011)
; (Many are marked. Code tied to references to *the-person* are by MA.)


;*********************************************************************************
;; GET/PUT MACROS

(defmacro activation (unit) `(get ,unit 'activation))
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
; Note doesn't know the difference between sym-links and one-way links.
(defmacro weight-of-link-between (unit1 unit2)
  `(or (cdr (assoc ,unit2 (links-from ,unit1))) 0)) ; i.e. if unit2 is listed in unit1's list of links, return the link weight, otherwise 0

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
; *THE-PERSON* MUST BE SET CORRECTLY.
(defun note-unit (unit &optional my-init-activ (person *the-person*))
  (setf (activation unit) (or my-init-activ *init-activ*)) ; sets activn even if nil is passed
  (setf (person-of unit) person)
  (pushnew unit (get person 'all-units)))

; Older version:
;(defun note-unit (unit)
;  (setf (activation unit) *init-activ*)
;  (pushnew unit (get *the-person* 'all-units)))

; MAKE-SYM-LINKS creates symmetric links between all of the members of a set
; of units.
; *THE-PERSON* MUST BE SET CORRECTLY: See comment on make-link for explanation.
(defun make-sym-links (list-of-units weight)
  (do ((units list-of-units (cdr units)))
      ((null units) nil)
    (dolist (unit (cdr units))
      (make-sym-link (car units) unit weight))))

; MAKE-SYM-LINK sets up a symmetric link between units.
; Important to make no links from unit to itself, and don't clobber
; excitatory links with inhibitory ones.  Also won't make an acme map
; unit unless it doesn't exist.
; Excitatory links sum, but inhibitory don't.
; This slows down creation of sym-links, but only has to be done once.
; *THE-PERSON* MUST BE SET CORRECTLY to update number of links in person in make-link.
; However, DOES NOT CONVERT GENERIC/PERSONAL ETC.: Uses whatever is passed in as the unit name.
; 4/20/2012 split the old OR into two cond tests.
(defun make-sym-link (unit1 unit2 weight)
  (if (not (eq unit1 unit2))                  ; if distinct nodes
    (let ((old-weight (weight-of-link-between unit1 unit2)))
      (cond ((= old-weight 0)             ; and either never-linked [MAYBE REPLACE WITH UNLINKED?]
             (mark-constraint-newly-added unit1 unit2 weight *the-person*) ; record that we're making a new constraint, so popco can tell gui if desired
             (raw-make-sym-link unit1 unit2 weight +acme-max-weight+)) ; then call make-link to create link, or update weight if existing excitatory link with unmarked units
            ((and (> old-weight 0)        ; or [excitatorily-linked
                  (do-sum-weights unit1)  ;    and neither node marked to avoid summing link weights
                  (do-sum-weights unit2)) ;    [popco avoids summing due to conversation.]
             (raw-make-sym-link unit1 unit2 weight +acme-max-weight+)))))) ; then call make-link to create link, or update weight if existing excitatory link with unmarked units

;; RAW-MAKE-SYM-LINK
;; Added 4/20/2012, replaced separate make-link calls in make-sym-link with this. -MA
; *THE-PERSON* MUST BE SET CORRECTLY to update number of links in person.
; And to record sym-links as new in person.
(defun raw-make-sym-link (unit1 unit2 weight &optional (max-weight 1) (min-weight -1))
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
(defun make-link (unit1 unit2 weight &optional (max-weight 1) (min-weight -1))
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

; RUN-EXP is the top-level function. It is called from the data file,
; usually as the last command. It sets the activation of the special unit,
; settles the network (by calling RUN-HYP-NET), and prints out the results.
; *THE-PERSON* MUST BE SET PROPERLY
(defun run-exp ()
  (print-si "Running COHERE connectionist algorithm.")
  (setf (activation 'special) *special-activation*)
  (my-print " *****")
  (run-hyp-net)
  (unless *silent-run?*
    (print-propns)))

; RUN-HYP-NET is the main loop used for settling the network. For each
; cycle it updates the activations of the units, checks for assymptoted
; units, and prints out useful information on it's status.
; *THE-PERSON* MUST BE SET PROPERLY
(defun run-hyp-net ()
  ; (unless *silent-run?* (print-values))
  (setf *testnum* (gensym "test"))
  (do ((timestep 1 (+ 1 timestep))
       (units (get *the-person* 'all-units))
       (old-asymptoted nil))
    ( (or *stop-run?*
          (and *stop-settled?* (get *the-person* 'settled?)) ; network has settled
          (> timestep *max-times*))
     (and (print-run 'verbose)
          ;(cond (*silent-run?* (princ '".") (finish-output)))
          )) ; Abbreviated. -MA 7/2011
    (if *grossberg?* ; use Grossberg's updating rule.
      (mapc #'update-unit-activn-gross units)  ; 11/2011 changed mapcar to mapc -MA
      ; else use Rumelhart & McClelland rule:
      (mapc #'update-unit-activn               ; 11/2011 changed mapcar to mapc -MA
            (set-difference units (get *the-person* 'evaluation-units))))
    ; note 6-23-2000 - remove evaluation units to prevent
    ; duplicate their updating [MA: I think this has to do with HOTCO]
    (update-valences) ; used by HOTCO - does evaluation units too
    ;*** BEGINNING of lines in which 'new-activation may be different from 'activation
    (when *report-activn-change*                      ; Optionally report total of |activation-new-activation|. Added 1/2012. -MA
      (let* ((changed-units (set-difference units (get *the-person* 'asymptoted-units)))
             (num-changed (length changed-units)))
        (format t "~S: ~S units with change above *asymptote*, avg change = ~,4f~%" *the-person* num-changed 
                (/ (sum-activn-changes changed-units) num-changed))
        ;(format t "~S~%" (mapcar #'abs-activn-change changed-units)) ; list the changed units
       ))
    (when (and *watched-nodes* (find *the-person* *watched-persons*)) ; if there are watched [generic-name] nodes, and we're watching this person
      (terpri)
      (mapc #'print-activation (mapcar #'generic-to-personal-sym *watched-nodes*)))
    (setf (get *the-person* 'settled?) t) ; has network reached asymptote? [fix-activation, called below, may undo this]
    ; this turns nil if unit not asymptoted.
    (setf old-asymptoted (get *the-person* 'asymptoted-units))
    (setf (get *the-person* 'asymptoted-units) nil)
    ;*** END of lines in which 'new-activation may be different from 'activation
    (mapc #'fix-activation units) ; 1/2012 changed mapcar to mapc -MA  [note can change value of 'settled?]
    (fix-valences) ; used by HOTCO
    (unless *silent-run?*
      (mapcar #'announce-asymptote
              (set-difference (get *the-person* 'asymptoted-units) old-asymptoted)))
    (if (and *stop-settled?* (get *the-person* 'settled?) (not *silent-run?*))
      (my-print '"Network has settled by cycle " (get *the-person* 'total-times) "."))
    ;(if (member timestep *when-to-print*) (print-propns))
    ; for graphics:
    ;(if *use-actgraph* (show-act))
    ; (if *trace-list* (update-trace)) ;?????????
    (setf (get *the-person* 'total-times) (+ (get *the-person* 'total-times) 1)))) ; total-activn-change = float if *report-total-activn-change*, else nil. added 1/2012 -MA





; UPDATE-UNIT-ACTIVN updates the activation of a unit based on the
; links it has.
(defun update-unit-activn (unit)
  (declare (ftype (function (&rest float) float) min max + * -)
           (ftype (function (float float) symbol) >))
  (let ((net-input-value (net-input unit)))
    (declare (type (float) net-input-value))
    (setf (new-activation unit)
          (min *max-activation*
               (max *min-activation*
                    (+ (* (activation unit) (- 1.0 *decay-amount*))
                       (if (> net-input-value 0.0)
                           (* net-input-value
                              (- *max-activation* (activation unit))
                              )
                         ; else:
                         (* net-input-value
                            (- (activation unit) *min-activation*)))))))))


; NET-INPUT is the weighted sum of output from all input units.
(defun net-input (unit)
  (declare (ftype (function (&rest float) float) max + *))
  (do ((links (links-from unit) (cdr links))
       (result 0.0))
      ((null links) result)
    (declare (type (float) result))
    (setf result (+ (* (float (cdar links))
                       (max *output-threshold* (activation (caar links))))
                    result))))


; FIX-ACTIVATION records the new activation and notes if the unit
; has reached asymptote.
(defun fix-activation (unit)
  (cond ((and *stop-settled?*
              (< (abs (- (new-activation unit)
                         (activation unit)))
                 *asymptote*)
              (>= (get *the-person* 'total-times) *min-settle*))
         (setf (get *the-person* 'asymptoted-units)
               (cons unit (get *the-person* 'asymptoted-units))))
        (t (setf (get *the-person* 'settled?) nil)))
  ; (my-print "Unit: " unit ", difference: " (- (new-activation unit) (activation unit)))
  (setf (activation unit) (new-activation unit)))



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
  (declare (ftype (function (&rest float) float) + - * min max))
  ; calculate excitation and inhibition.
  (if *tversky?*
      (excit-and-inhib-tversky unit)
      (excit-and-inhib unit))
  (setf (new-activation unit)
        (min *max-activation*
             (max *min-activation*
                  (+ (* (activation unit) (- 1.0 *decay-amount*))
                     (* (- *max-activation* (activation unit)) *current-excit*)
                     (* (- (activation unit) *min-activation*) *current-inhib*))))))

; EXCIT-AND-INHIB is just like net-input, except that it keeps track
; of excitation and inhibition separately. EXCIT-AND-INHIB-TVERSKY is
; identical except that units with negative activation can pull down
; their excitatory neighbors.
(defun excit-and-inhib (unit)
  (declare (ftype (function (&rest float) float) max + *)
           (ftype (function (float float) symbol) >))
  (do ((excit 0.0) (inhib 0.0) (wt 0.0) (activn 0.0)
       (links (links-from unit) (cdr links)))
      ((null links)
       (setf *current-excit* excit)
       (setf *current-inhib* inhib))
    (declare (type (float) excit inhib wt activn))
    (setf wt (float (cdar links)))
    (setf activn (max *output-threshold* (activation (caar links))))
    (if (> wt 0.0)
        (setf excit (+ excit (* wt activn)))
      ; else wt is inhibitory:
      (setf inhib (+ inhib (* wt activn))))))

(defun excit-and-inhib-tversky (unit)
  (declare (ftype (function (&rest float) float) max + *)
           (ftype (function (float float) symbol) >))
  (do ((excit 0.0) (inhib 0.0) (wt 0.0)
       (links (links-from unit) (cdr links)))
      ((null links)
       (setf *current-excit* excit)
       (setf *current-inhib* inhib))
    (declare (type (float) excit inhib wt))
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
(defun show-sym-link (unit1 unit2)
  (list (assoc unit2 (links-from unit1))
        (assoc unit1 (links-from unit2))))

; SET-SYM-LINK-WEIGHT: Set weight of an existing sym-link. Added by MA 11/2011.
(defun set-sym-link-weight (unit1 unit2 weight)
  (let ((unit1-link (assoc unit2 (links-from unit1)))
        (unit2-link (assoc unit1 (links-from unit2))))
    (if (not (and unit1-link unit2-link))
      (error "set-sym-link-weight: Can't set sym-link weight to ~S. No two-way link between ~S and ~S."
             weight unit1 unit2))
    (setf (cdr unit1-link) weight)
    (setf (cdr unit2-link) weight)))

; Convenience function to set sym link weight from the representation
; produced by list-constraints and sometimes stored in person 
; property all-constraints.  Added by MA 12/11
(defun set-sym-link-weight-from-constraint (constraint)
  (set-sym-link-weight (car constraint) (cadr constraint) (cddr constraint)))
; tip - example:
;(mapc #'set-sym-link-weight-from-constraint (mapcar #'maybe-personalize-constraint list-of-constraints))

; Added by MA 11/11
(defun sym-linked? (unit1 unit2)
  (and (assoc unit2 (links-from unit1))
       (assoc unit1 (links-from unit2))))

; Note unlinked? is not equivalent to not sym-linked? if assymetric links are in use.
(defun unlinked? (unit1 unit2)
  (not (or (assoc unit2 (links-from unit1))
           (assoc unit1 (links-from unit2)))))

; Old COHERE-style test for being unlinked.  
; Fails on POPCO's zero-weight proposition/salient links.
(defun cohere-style-unlinked? (unit1 unit2)
  (= 0 (weight-of-link-between unit1 unit2)))

(defun abs-activn-change (unit)
  (abs 
    (- (get unit 'activation) 
       (get unit 'new-activation))))

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
