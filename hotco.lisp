; FILE: hotco.lisp
; PURPOSE: Emotional coherence
; PROGRAMMER: Paul Thagard
; CREATED: July 7, 1997
; UPDATED: 1-28-1998
; UPDATED: 6-21-2000, for HOTCO 2



; Remarks. In ACME, IMP, ECHO, activation(unit)=f(activations inputs).
; In EMOCO, valence(unit)=f(activations and valences of inputs).
; Wishful thinking would be: activation(unit)=f(activations and valences)
; - weaker version implemented in HOTCO 2
; Activation represents confidence (truth).
; Valence represents desirability (utility).

; HOTCO sets up variables for running emotional coherence.

(defun hot ()
  (setf *emote* t)
  (setf (get 'special 'valence) 0L0)
  (my-print "Running in HOT cognition mode.")
  )



; VALENCE-UNIT creates a new unit if necessary and provides it with
; valence links to a specified list of units.
; This can also be used to create a special unit that gives emotional
; support to the specified list of elements.
; The unitlist consists of pairs (unit magnitude) where magnitude
; indicates the strength of the emotional connection between the units.
; This function provides valence inputs to somatically linked units,
; and also can be used to establish an output unit to be evaluated.
; The type is either input or output.

(defun valence-unit (unit unitlist)
  (pushnew unit (get *the-person* 'all-valence-units))
  (pushnew unit (get *the-person* 'all-structures))
  ; initialize valence and activation
  (cond ((equal unit 'valence-special)
         (setf (get unit 'valence) 1.0)
         (setf (get unit 'activation) 1.0)
         (setf (get 'valence-special 'valence) 1)
         )
        (t (setf (get unit 'valence) 0.01) ; else low default
           (setf (get unit 'activation) .01)
           (pushnew unit (get *the-person* 'all-units)) ; for HOTCO 2
           )
        )
  (dolist (pair unitlist)
    (my-print "Valence link of magnitude " (second pair)
              " between " unit " and " (first pair)
              )
    ; initialize valences
    (setf (get (first pair) 'valence) 0.01)
    ; make the valence links
    (valence-link unit (first pair) (second pair))
    (pushnew (first pair) (get *the-person* 'all-valence-units))
    (pushnew (first pair) (get *the-person* 'all-units)) ; for HOTCO 2
    )
  )

; VALENCE-LINK adds a valence link between two units.

(defun valence-link (unit1 unit2 magnitude)
  (let ((valence-weight (* magnitude *valence-weight*)))
    (make-valence-link unit1 unit2 valence-weight)
    (make-valence-link unit2 unit1 valence-weight)
    )
  )





;; GET/PUT MACROS

; VALENCE
(defmacro valence (unit) `(get ,unit 'valence))

; NEW-VALENCE
(defmacro new-valence (unit) `(get ,unit 'new-valence))

; VALENCE-LINKS
(defmacro valence-links (unit) `(get ,unit 'valence-links))

; SHOW-VALENCE
(defun show-valence ()
  (mapcar #'print-plist (get *the-person* 'all-valence-units))
  )

; MAKE-VALENCE-LINK

(defun make-valence-link (unit1 unit2 weight)
  (setf (get unit1 'valence-links)
        (acons unit2 weight (valence-links unit1))
        )
  (pushnew unit1 (get *the-person* 'all-valence-units))
  (pushnew unit2 (get *the-person* 'all-valence-units))
  )

; UPDATE-VALENCES is called within RUN-HYP-NET to update valences
; if *emote* is turned on. Like activations, valences can range between
; 1 and -1.

(defun update-valences ()
  (if *emote* (mapcar #'update-valence
                      (union (get *the-person* 'all-units)
                             (remove 'valence-special (get *the-person* 'all-valence-units))
                             )
                      )
    )
  ; for HOTCO 2
  (if *emote* (mapcar #'update-eval-activn (get *the-person* 'evaluation-units)))
  )

; UPDATE-VALENCE is like UPDATE-UNIT-ACTIVN, except that
; VALENCE-INPUT differs from NET-INPUT in that it combines both
; activation and valence. If unit1 is valence linked to unit2, then
; the valence of unit1 is affected by a multiplicative
; combination of the valence and the activation of unit2.
; Note that the updating of valence to new-valence takes place
; in FIX-VALENCES which ensures asynchronous updating - the valence
; of a unit is updated based on the valences and activations of units
; at the last timestep.

(defun update-valence (unit)
  (declare (ftype (function (&rest float) float) min max + * -)
           (ftype (function (float float) symbol) >))
  (let ((net-valence-value (valence-input unit)))
    (declare (type (float) net-valence-value))
    (setf (new-valence unit)
          (min *max-activation*
               (max *min-activation*
                    (+ (* (valence2 unit) (- 1.0 *decay-amount*))
                       (if (> net-valence-value 0.0)
                           (* net-valence-value
                              (- *max-activation* (valence2 unit))
                              )
                         ; else:
                         (* net-valence-value
                            (- (valence2 unit) *min-activation*)
                            )
                         )
                       )
                    )
               )
          )
    )
  )

; VALENCE2 is a kludge to cover cases where units do not have
; any valence to start with.

(defun valence2 (unit)
  (or (get unit 'valence)
      .01
      )
  )



; VALENCE-INPUT differs from NET-INPUT in that it combines both
; activation and valence. If unit1 is valence linked to unit2, then
; the valence of unit1 is affected by a multiplicative
; combination of the valence of unit2, the activation of unit2,
; and the weigth between unit1 and unit2.
; Note that the input links include BOTH explicit valence links
; and the regular activation links.
; 12-1-98: Fixed so that valence comes from valence links if
; they exist, and activation links otherwise.

(defun valence-input (unit)
  (do ((links (union (get unit 'valence-links)
                     (get unit 'links-from) ; activation links
                     )
              (cdr links)
              )
       (result 0.0)
       )
      ((null links) result)
    (setf result (+ (* (float (cdar links))
                       (or (activation (caar links)) .01)
                       (or (valence (caar links)) .01)
                       )
                    result
                    )
          )
    ;(my-print (car links) " RES " result)
    )
  )





; FIX-VALENCES is analogous to FIX-ACTIVATION, ensuring
; asynchronous updating - the valence
; of a unit is updated based on the valences and activations of units
; at the last timestep.

(defun fix-valences ()
  (if *emote*
      (dolist (unit (union (remove 'valence-special (get *the-person* 'all-valence-units))
                           (get *the-person* 'all-units)
                           )
                    )
        (setf (valence unit) (new-valence unit))
        )
    )
  )



;;;;;;;;;;;;;;;;; HOTCO 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;

; In the original HOTCO, activations influenced valences but not vice versa.
; In HOTCO, valences can influence activations for a special subset of units
; called "evaluation units" which intrinsically involve an evaluation.
; For example, the truth of "OJ is good" is linked with the valence of OJ.

; UPDATE-EVAL-ACTIVN updates the activation of an evaluation unit based on the
; links it has. It differs from UPDATE-UNIT-ACTIVN in that
; the activation of a unit is a function of both its net input activation
; AND its net valence activation. Here an average is taken.

(defun update-eval-activn (unit)
  (declare (ftype (function (&rest float) float) min max + * -)
           (ftype (function (float float) symbol) >))
  (let ((net-input-value (net-eval-input unit)))
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
                            (- (activation unit) *min-activation*)
                            )
                         )
                       )
                    )
               )
          )
    )
  )

; NET-EVAL-INPUT combines activation and valence input

(defun net-eval-input (ut)
  (+ (net-input ut) (valence-input ut))
  )




