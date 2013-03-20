; File: cohere.lisp
; Purpose: combine coherence programs and test different algorithms
; Programmer: Paul Thagard, 5-95

; variables - see variables.lisp



; EVAL-COHERE compares 4 different algorithms for accepting and rejecting
; elements based on coherence.
; 1. COH-COUNT generates all possible solutions and ranks the ones that
; best maximize the total weight W of constraints.
; 2. COH-POINT-5 is an incremental approximation algorithm that guarantees that at
; least .5 of the achievable W will be achieved.
; 3. COH-POINT-878 is an approximation algorithm that guarantees that at
; least .878 of the achievable W will be achieved. Not implemented here.
; 4. CONNECT-SOLUTION uses a connectionist algorithm to accept and reject elements.
; This function assumes that a network of constraints has already been set up by
; ECHO, DECO, ACME, IMP, or other coherence program.
; 5. GREEDY uses a greedy algorithm to maximize coherence.
; For emotional coherence, HOTCO, only exhaustive and connectionist algorithms are used.

(defun EVAL-COHERE ()
  (my-print "=====================")
  (my-print "Evaluating coherence for problem " *problem* " in mode " *eval-mode*)
  (time (coh-count *eval-mode*)) ; mode is pure, tempered, or foundational
  (time (coh-point-5))
  (time (greedy *greedy-flips*)) ; use greedy algorithm with # flips
  (time (connect-solution))
  (my-print "*********************")
  (my-print "Elements: " (get *the-person* 'all-units))
  (my-print "Constraints: " (get *the-person* 'all-constraints))
  (my-print "Coherence solutions selected for problem: " *problem*)
  (my-print "solution %weight %constraints accepted rejected")
  (my-print "Exhaustive solution(s):")
  (print-solutions *count-solution*)
  (my-print "Incremental solution:")
  (my-print *point-5-solution*)
  (my-print "Greedy solution:")
  (my-print *greedy-solution*)
  (my-print "Connectionist solution: ")
  (my-print *connect-solution*))

; PURE, TEMPERED, FOUNDATIONAL

(defun pure ()
  (setf *eval-mode* 'pure)
  (setf *special-activation* *init-activ*) ; set to .01
  )
(defun temp ()
  (setf *eval-mode* 'tempered)
  (setf *special-activation* *special-register*) ; back to 1
  )

(defun found ()
  (setf *eval-mode* 'foundational)
  (setf *special-activation* *special-register*)
  ) ; and *all-data* too.

;==================== COH-COUNT ===========================

; COH-COUNT does coherence maximization in the intractably
; exhaustive way. A solution is a list (number %1 %2 accepted-list rejected-list)
; where number is the number of the solution, %1 is the decimal fraction of
; the total weight of constraints satisfied, and %2 is the fraction of
; the total number of constraints satisfied.
; Coherence can be counted in one of three modes:
; Pure: no favored elements.
; Tempered: favored elements linked to special unit, which is accepted.
; Foundational: all favored elements are accepted.

(defun coh-count (mode)
  (my-print "Computing coherence using the exhaustive counting algorithm.")
  (let ((solutions (gen-solutions mode))
        (best nil)
        )
    (setf (get *the-person* 'all-constraints) (list-constraints (get *the-person* 'all-units)))
    (setf solutions (coh-score-all solutions))
    (print-solutions solutions)
    (setf best (best-solution solutions))
    (my-print "Best counting solution(s): " (print-solutions best))
    (if (and (car solutions)
             (= (second (car solutions)) (second (second solutions))) ; tie
             )
        (my-print "NOTE: " (length best) " solutions are tied.")
      )
    (setf *count-solution* best)
    )
  )

; GEN-SOLUTIONS generates all possible partitions of a set of units into
; accepted and rejected. The input (get *the-person* 'all-units) comes from ECHO or
; other coherence program.

(defun gen-solutions (mode)
  (my-print "There are "
            (setf *num-solutions* (expt 2 (length (get *the-person* 'all-units))))
            " pure solutions to " *problem*
            )
  (my-print "Generating solutions in " mode " mode.")
  (if (equal mode 'foundational)
      (my-print "There are "
                (setf *num-solutions*
                      (expt 2 (length (set-difference (get *the-person* 'all-units) (get-favored))))
                      )
                " foundational solutions to " *problem*
                )
    )
  (cond ((> *num-solutions* *max-num-solutions*)
         (my-print "TOO MANY POSSIBLE SOLUTIONS.")
         nil ; quit if too big
         )
        (t
         ; otherwise loop:
         (do ((element-lists (powerlist (get *the-person* 'all-units)) (cdr element-lists))
              (result nil)
              (count 1 (1+ count))
              (favored (get-favored))
              (accepted-els nil)
              )
             ((null element-lists) result) ; return
           ; repeat:
           (unless (and (equal mode 'foundational) ; if foundational, ignore solutions that
                        (not (subsetp favored (car element-lists))) ; don't accept favored elements
                        )
             (push (list count ; number of solution
                         ; accepted elements:
                         (setf accepted-els
                               (cond ((equal mode 'pure) (car element-lists))
                                     ((equal mode 'tempered)
                                      (cons 'special (car element-lists)) ; special is accepted
                                      )
                                     ((equal mode 'foundational) ; favored are accepted
                                      (cons 'special (union favored (car element-lists)))
                                      )
                                     )
                               )
                         ; rejected elements:
                         (set-difference (get *the-person* 'all-units) accepted-els) ; rejected
                         )
                   result
                   )
             )
           ) ; end do
         )) ; end cond
  )

; GET-FAVORED returns a list of all favored elements, i.e. ones with links to SPECIAL

(defun get-favored ()
  (mapcar #'car (links-from 'special))
  )

; COH-SCORE-ALL revises a list of solutions of the form (number accepted rejected)
; and returns a ranked list of the form (number %weight %constraints accepted rejected)
; The score is a list of the two %.

(defun coh-score-all (lst)
  (setf *weight-of-all-constraints* (sum-constraints))
  (do ((input lst (cdr input))
       (output nil)
       (score nil)
       )
      ((null input) (sort-solutions output)) ; return
    ; repeat:
    (setf score (coh-score (car input)))
    (push (list (caar input) ; number
                (car score) ; %weight
                (second score) ; %constraints
                (second (car input)) ; accepted
                (third (car input)) ; rejected
                )
          output
          )
    )
  )

; SORT-SOLUTIONS sorts a list of solutions according to %weight satisfied.

(defun sort-solutions (lst)
  (sort lst #'> :key #'second))

; LIST-CONSTRAINTS generates a list of all constraints using the links associated
; with each unit (element) in a list. Each constraint has the structure
; (unit1 unit2 weight). To avoid duplicating constraints (the links are
; symmetrical), it ignores constraints with units already checked.

(defun list-constraints (list-of-units)
  (do ((units list-of-units (cdr units))
       (constraints nil)
       (unit nil)
       (units-done nil)) ; variables
    ((null units) constraints) ; result
    ; repeat:
    (dolist (link (get (car units) 'links-from))
      (setf unit (car link))
      (unless (member unit units-done) ; constraint already noted
        (push (cons (car units) link) constraints)))
    (push (car units) units-done)))

; SUM-CONSTRAINTS calculates the sum of all the weights on the constraints
(defun sum-constraints ()
  (apply #'+ (mapcar #'abs-third (get *the-person* 'all-constraints))))

; ABS-THIRD takes the absolute value of the third of a list
; third function doesn't work because a link is a dotted pair
(defun abs-third (lst)
  (abs (cddr lst)))





; COH-SCORE calculates the coherence scores
; of a set of units partitioned into accepted and
; rejected. It returns a list of two scores
; %weight - the fraction of the total weight satisfied
; %constraints - the fraction of total constraints satisfied
; Input is a triple (number accepted rejected)

(defun coh-score (solution)
  (let ((num-satisfied 0)
        (weights-satisfied 0)
        (num-constraints (length (get *the-person* 'all-constraints)))
        (constraint-weight 0))
    (dolist (constraint (get *the-person* 'all-constraints))
      (setf constraint-weight
            (cons-satisfied constraint (second solution) (third solution)))
      (if (> constraint-weight 0)
          ; if constraint is satisfied by the accepted/rejected partition, then
        (and (setf num-satisfied (1+ num-satisfied))
             (setf weights-satisfied (+ constraint-weight weights-satisfied)))))
    (list (roundoff 3 (float (/ weights-satisfied *weight-of-all-constraints*))) ; %weight
          (roundoff 3 (float (/ num-satisfied num-constraints)))))) ; %constraints


; ROUNDOFF

(defun roundoff (places num)
  "Rounds NUM to PLACES decimal places."
  (cond ((eq places 0) (round num))
        (t (/ (round (* num (expt 10.0 places))) (expt 10.0 places)))
        )
  )

; CONS-SATISFIED checks to see if a constraint is satisfied, where a constraint
; has the forum (unit1 unit2 weight). If the weight is positive, then the units
; should be either both accepted or both rejected. If the weight is negative,
; then one unit should be accepted and the other rejected.
; Number returned is 0 if constraint is not satisfied, and
; the weight on the contrainst unless the constraint is positive with
; both elements accepted and *resolution-impact* is > 1.

(defun cons-satisfied (const acc rej)
  (cond ((and (> (cddr const) 0) ; positive constraint affirmatively satisfied
              (member (first const) acc)
              (member (second const) acc)
              )
         (* (cddr const) *resonance-impact*) ; add resonance factor
         )
        ((and (> (cddr const) 0) ; positive constraint negatively satisfied
              (member (first const) rej)
              (member (second const) rej)
              )
         (cddr const)
         )
        ; else negative constraint:
        ((and (< (cddr const) 0) ; negative constraint
              (or (and (member (first const) acc)
                       (member (second const) rej)
                       )
                  (and (member (first const) rej)
                       (member (second const) acc)
                       )
                  )
              )
         (abs-third const) ; absolute value of weight
         )
        (t 0) ; otherwise 0
        )
  )

; RESON

(defun reson (num)
  (setf *resonance-impact* num)
  
  )

; PRINT-SOLUTIONS prints *show-solutions* out neatly

(defun print-solutions (lst)
  (do ((solutions lst (cdr solutions))
       (count 1 (1+ count))
       )
      ((or (> count *show-solutions*) (null solutions))
       (terpri) (my-print "No more than " *show-solutions* " solutions are shown.")
       )
    (print (car solutions))
    )
  )

; BEST-SOLUTION prints out the best solution, or the best solutions if there are ties.

(defun best-solution (solns)
  (if (null solns) nil
    (do ((solutions (cdr solns) (cdr solutions))
         (result (list (car solns)))
         )
        ((/= (second (car solns)) (second (car solutions))) ; no tie
         (sort (mapcar #'resonate result) #'> :key #'last-el) ; return ordered by resonance
         )
      ; repeat
      (push (car solutions) result)
      )
    )
  )

; RESONATE assesses the resonance of a solution, i.e. the extent to which
; it accepts elements that positively constrain each other.
; The resonance becomes the sixth entry in the solution.

(defun resonate (soln)
  (do ((accepted (fourth soln) (cdr accepted))
       (count 0)
       )
      ((null accepted) (append soln (list count))) ; return
    ; repeat
    (setf count (+ count ; count number of positivley linked accepted units
                   (length (intersection (linked-to (car accepted) 'positive)
                                         (cdr accepted)
                                         )
                           )
                   )
          )
    )
  )

; LINKED-TO returns the units linked to a given unit, selecting either
; positive, negative or all

(defun linked-to (unit selection)
  (do ((links (links-from unit) (cdr links))
       (result nil)
       )
      ((null links) result) ; return
    ; repeat
    (cond ((and (equal selection 'positive)
                (> (cdr (car links)) 0)
                )
           (push (caar links) result)
           )
          ((and (equal selection 'negative)
                (< (cdr (car links)) 0)
                )
           (push (caar links) result)
           )
          ((equal selection 'all)
           (push (caar links) result)
           ) ; include all units linked
          )
    )
  )

; =============== Karsten's approximating algorithm ===========

; COH-POINT-5 uses a serial algorithm to accept or reject units.

(defun coh-point-5 ()
  (my-print "Computing coherence using incremental algorithm.")
  (do ((units (randomize (get *the-person* 'all-units)) (cdr units)) ; put units in random order
       (accepted '(special))
       (rejected nil)
       (unit nil)
       (units-done nil)
       ) ; variables
    ((null units) ; result
     (setf *point-5-solution*
           (cons 'incremental (append (coh-score (list 'incremental accepted rejected))
                                      (list accepted rejected)
                                      )
                 )
           )
     (my-print "Incremental algorithm chose solution " *point-5-solution*)
     )
    ;repeat:
    (setf unit (car units))
    (push unit units-done)
    (if (>= (coh* (cons unit accepted) rejected units-done)
            (coh* accepted (cons unit rejected) units-done)
            ) ; coherence is greater if unit is accepted
      (and (my-print "Accepting " unit)
           (push unit accepted)
           )
      ; else reject unit
      (and (push unit rejected)
           (my-print "Rejecting " unit)
           )
      )
    )
  )

;; RANDOMIZE rewrites a list in random order
;; DEPRECATED: Use shuffle in popco-utils.lisp instead.
(defun randomize-deprecated (list)
  (do ((lst list )
       (num-list nil) ; association list
       (result nil)
       (selected nil)
       (total 0))
      ((null lst) result) ; return
    ; repeat
    (setf num-list (assoc-nums lst)) ; make new association list
    (setf total (length lst))
    (setf selected (cdr (assoc (random total) num-list))) ; select random pair
    (push selected result)
    (setf lst (remove selected lst))))

; ASSOC-NUMS produces an association list of (number unit). This is like make-units
; above.

(defun assoc-nums (list)
  (do ((lst list (cdr lst))
       (result nil)
       (num 0 (1+ num)))
      ((null lst) result) ; return
    (setf result (acons num (car lst) result))))

; COH* calculates the coherence of a set of units partitioned into accepted and
; rejected.

(defun coh* (acc rej all)
  (let ((num-satisfied 0)
        (constraints (list-constraints all))
        (num-constraints 0))
    (dolist (constraint constraints )
      
      (if (cons-satisfied* constraint acc rej)
          (setf num-satisfied (1+ num-satisfied))))
    (setf num-constraints (length constraints))
    (if (zerop num-constraints) 1 ; else
      (float (/ num-satisfied num-constraints)))))

; CONS-SATISFIED* is like cons-satisfied.

(defun cons-satisfied* (const acc rej)
  (if (> (cddr const) 0) ; positive constraint
    (or (and (member (first const) acc)
             (member (second const) acc)
             )
        (and (member (first const) rej)
             (member (second const) rej)
             )
        )
    ; else negative constraint:
    (or (and (member (first const) acc)
             (member (second const) rej)
             )
        (and (member (first const) rej)
             (member (second const) acc)
             )
        )
    )
)









; RANDOM-UNIT selectes a unit at random from the association list.
(defun random-unit ()
  (assoc (random (get *the-person* 'total-units))
         (get *the-person* 'number-units)))

; ================== CONNECTIONIST =========



; CONNECT-SOLUTION translates a settled network into a solution

(defun connect-solution ()
  (my-print "Computing coherence using connectionist algorithm.")
  (setf (get 'special 'activation) *special-activation*)
  (run-hyp-net)
  (setf *connect-solution*
        (do ((units (get *the-person* 'all-units) (cdr units))
             (acc nil)
             (rej nil))
            ((null units)
             (cons 'connectionist
                   (append (coh-score (list 'connect (cons 'special acc) rej))
                           (list (cons 'special acc) rej)))); return
          (if (accepted (car units))
              (push (car units) acc)
            ; else
            (push (car units) rej))))
  (my-print "Connectionist solution is: " *connect-solution*))



; ACCEPTED yields T if a unit's activation is above 0.
; REJECTED-CONNECT yields T if a unit's activation is below 0.

(defun accepted (unit)
  (> (get unit 'activation) 0)
)

(defun rejected (unit)
  (< (get unit 'activation) 0)
)
; ================= .878 ===================
; Input: list of constraints, each of which is a list:
; (unit1 unit2 weight)
; Negative weights indicate negative constraints.
; Output: list of two lists:
; Accepted (must include the unit SPECIAL)
; Rejected
; Output represents the partition that maximizes the total weight
; of the constraints satisfied (see formal definition)



; =================UTILITIES================
; NAME-UNIT creates a new symbol

(defun name-unit (str num)
  (make-symbol (coerce (append (coerce str 'list)
                               (coerce (princ-to-string num) 'list)
                               )
                       'string
                       )
               )
)

;(defun pls () (mapcar 'pl (get *the-person* 'all-units)))

(defun end-at (num)
  (setf *max-times* num))

(defun put (atom property value)
  (setf (get atom property) value))

; POWERLIST generates all subsets of a list

(defun powerlist (lst)
  (cond ((null lst) nil)
        ((= (length lst) 1)
         (list lst '())
         )
        (t (append (powerlist (cdr lst))
                   (push-all (car lst) (powerlist (cdr lst)))
                   )
           )
        )
  )

; PUSH-ALL(E, LofL) returns a list of lists consisting of lists made by;
; consing E on to each member of LofL

(defun push-all (el list-of-lists)
  (do ((lst list-of-lists (cdr lst))
       (result nil)
       )
      ((null lst) result) ; return
    (push (push el (car lst)) result)
    )
  )

