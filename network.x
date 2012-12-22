

;; PT's VERSION BEFORE MARSHALL'S MODS:
;(defun make-sym-link (unit1 unit2 weight)
;  (cond ( (and (not (eq unit1 unit2))                        ; if distinct units
;               (>= (weight-of-link-between unit1 unit2) 0))  ; and either unlinked [0] or excitatory [>0]
;         ; then:
;         (make-link unit1 unit2 weight)
;         (make-link unit2 unit1 weight))))

;; Previous version by PT
;(defun make-sym-link (unit1 unit2 weight)
;  (cond ((and (not (eq unit1 unit2))                    ; if distinct units
;	   (= (weight-of-link-between unit1 unit2) 0))  ; and either unlinked [0] or excitatory [>0]
;	   (>= (weight-of-link-between unit1 unit2) 0))  ; and either unlinked [0] or excitatory [>0]
;         ; then:
;         (make-link unit1 unit2 weight)
;         (make-link unit2 unit1 weight))

;; MA's fall 2011 failed experiment: avoided adding weights when
;; looping through POPCO generations, but also in situations when weights
;; should be added.
;(defun make-sym-link (unit1 unit2 weight)
;  (let ((old-weight (weight-of-link-between unit1 unit2)))
;    (if (not (eq unit1 unit2))                ; if distinct nodes
;      (cond ((or (= old-weight 0)             ; and either never-linked
;                 (and (> old-weight 0)        ; or [excitatorily-linked
;                      (not-acme-unit unit1)   ;    and not ACME nodes]
;                      (not-acme-unit unit2)))
;             ;(note-sym-link unit1 unit2 weight) ; record in person's constraint list [Currently done en masse with list-constraints]
;             (make-link unit1 unit2 weight)      ; then call make-link
;             (make-link unit2 unit1 weight)))))) ; to create link or update weight if non-acme
;
;; This version supposed to be same as preceding, and clearer, but isn't right.  -MA
;(defun make-sym-link (unit1 unit2 weight)
;     (cond ((not (or                                                 ; if the units are neither
;	           (eq unit1 unit2)                                  ; identical, nor 
;		   (< (weight-of-link-between unit1 unit2) 0)        ; inhibitory, nor
;		   (and (is-acme-unit unit1) (is-acme-unit unit2)))) ; acme map units
;	    (make-link unit1 unit2 weight)                           ; then call make-link
;	    (make-link unit2 unit1 weight))))                        ; to create link, or update weight if non-acme

; NOTE-SYM-LINK records a new sym-link in *the-person*'s list of constraints.
; In general this should *only* be called from make-sym-link or a similar
; function--anything else invites inconsistent data structures.
; Future: replace pushnew with hashtable test?  This is an inefficient list search.
; *THE-PERSON* MUST BE SET CORRECTLY.
;(defun note-sym-link (unit1 unit2 weight)
;  (pushnew `(,unit1 ,unit2 . ,weight)
;           (get *the-person* 'all-constraints)
;           :test #'eq-constraint-ends))

; Note replacd form above in make-link replaces this one from original PT et al. code:
;(nsubstitute (cons unit2 (+ weight (cdr old))) old   ; replace old link entry in unit1's links-from 
;             (links-from unit1)))                    ;  with an entry with the new weight added in
; New version doesn't seem any faster, though it could have been, but is easier to read.
; It means same as: (setf (cdr old) (+ weight (cdr old))) .
;
; Here is PT's original version (with added comments)
;(defun make-link (unit1 unit2 weight)
;  (let (old)
;    (cond ((setf old (assoc unit2 (links-from unit1)))          ; if there's already (at least one-way) link from(?) unit2 to unit1
;           (nsubstitute (cons unit2 (+ weight (cdr old))) old   ; replace old link entry in unit1's links-from 
;                        (links-from unit1)))                    ;  with an entry with the new weight added in
;          (t                                                    ;  NOTE no attempt to place a max on link weight--can even exceed 1. -MA 3/12
;            (setf (links-from unit1)                            ; otherwise just create a new links-from entry
;                  (acons unit2 weight (links-from unit1)))
;            (setf (get *the-person* 'total-links) (1+ (get *the-person* 'total-links)))))))

;(defun WEIGHT_OF_LINK_BETWEEN (unit1 unit2)
;  (weight-of-link-between unit1 unit2))

; pre-4/20/2012 version [which worked]:
;(defun make-sym-link (unit1 unit2 weight)
;  (if (not (eq unit1 unit2))                  ; if distinct nodes
;    (let ((old-weight (weight-of-link-between unit1 unit2)))
;      (cond ((or (= old-weight 0)             ; and either never-linked
;                 (and (> old-weight 0)        ; or [excitatorily-linked
;                      (do-sum-weights unit1)  ;    and neither node marked to avoid summing link weights
;                      (do-sum-weights unit2)));    [popco avoids summing due to conversation.]
;             (raw-make-sym-link unit1 unit2 weight)))))) ; then call make-link to create link, or update weight if existing excitatory link with unmarked units

; ADD-WEIGHT
; I'm defining this as a macro for efficiency.
; It's intended to be used only in make-link.
; The value of +max-weight+ should be changed only in variables.lisp
; where it is defined.  Doing otherwise is asking for trouble.
(defmacro add-weight (addl-weight old-weight)
  (if +max-weight+ 
    `(min +max-weight+ (+ ,addl-weight ,old-weight))
    `(+ ,addl-weight ,old-weight)))


    (if *grossberg?* ; use Grossberg's updating rule.
      (mapc #'update-unit-activn-gross units)  ; 11/2011 changed mapcar to mapc -MA
      ; else use Rumelhart & McClelland rule:
      (mapc #'update-unit-activn               ; 11/2011 changed mapcar to mapc -MA
            (set-difference units (get *the-person* 'evaluation-units))))

; like OR, but a function to which a list of booleans can be passed
(defun list-or (lis)
  (if (null lis)
    nil
    (or (car lis)
        (list-or (cdr lis)))))

(defun settle-person-analogy-net (person)
  (unless (get person 'analogy-net-settled?)
    (setf (get person 'analogy-net-settled?) 
          (and
            (settle-net (get person 'all-map-units))
            (>= *pop-tick* *min-pop-ticks-settling*))))) ; don't allow settled? too early [NEEDS REVISION WHEN REPRODUCTION IS IMPLEMENTED]

(defun settle-person-propn-net (person)
  (unless (get person 'propn-net-settled?)
    (setf (get person 'propn-net-settled?) 
          (and
            (settle-net (get person 'all-propositions))
            (>= *pop-tick* *min-pop-ticks-settling*))))) ; don't allow settled? too early [NEEDS REVISION WHEN REPRODUCTION IS IMPLEMENTED]
