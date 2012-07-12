; FILE: echo.lisp
; PURPOSE: hybrid of echo and echo2, to allow switching between the two
; PROGRAMMER: Paul Thagard
; BREEDER: David Gochfeld
; CREATED: 6-21-90; adapted to COHERE by PT, 5-1-95
; UPDATED: 6-21-2000, PT, to experiment with make-compet-link

; This file is mostly ECHO2. Functions that are functionally identical
; in both ECHO and ECHO2 appear here in their ECHO2 form. If they are
; functionally identical but have different names, the ECHO2 name is
; used. Functions that have the same name but function differently
; between ECHO and ECHO2 appear as a single function with an IF
; statement testing the flag *echo2-mode*.
; My goal was to have the minimum amount of code that would allow the user
; to switch between ECHO and ECHO2 runs in the same session, without
; in any way affecting the results from either version. Thus, some functions
; are a little clunky, but hopefully their behavior for the respective
; version has not been affected.

; *******************************************************************
; Explanatory coherence as constraint satisfaction.
; *******************************************************************
; Principles:
; If hypotheses Hi together explain data D,
; make excitatory links between each of the Hi and D and
; make excitatory links between each pair of the Hi.
; Weight is proportional to the number of Hi.
; Make inhibitory links between each contradictory Hi and Hj.
; Have excitatory links between higher hypotheses and hypotheses they
; explain also.
; Analogy also establishes excitatory links.

; System ECHO.2: explanatory coherence by harmany (sic) optimization
; This version differs from the original ECHO in that
; it sets up incoherencies based on Principle C of TEC.2.
; Competition is noted automatically: two hypotheses that
; both explain the same evidence compete unless there is some
; explanatory relation between them.



;(if *echo2-mode*
; (my-print "You are running ECHO.2, which differs from the original ECHO.")
; (my-print "You are running the original ECHO.")
;)



(defvar *competitors* nil)
(defvar *common-hyps* nil)
(defvar *start-links* 0)

; DEFAULTS-ECHO

(defun defaults-echo2 ()
  (decay .05)
  (excit .04)
  (inhib -.06)
  (wtp '(120))
  (output -.99)
  (data-excit .05)
  (gross-off))

(defun dfe2 () (defaults-echo2))

; ***************************************************************
; Functions for parsing input.
; ***************************************************************
; PROPOSITION
(defun proposition (name sentence)
  (print-si name " stands for: " sentence)
  (setf (get name 'propn) sentence)
  (setf (get *the-person* 'all-propositions) (cons name (get *the-person* 'all-propositions)))
  (make-explan-unit name)
  (setf (get name 'activation) *init-activ*)
  (setf (get name 'original-activation) *init-activ*)
  )

; ***************************************************************
; CONTRADICT

(defun contradict (prop1 prop2)
  (make-explan-unit prop1)
  (make-explan-unit prop2)
  (print-si prop1 '" contradicts " prop2 '".")
  (make-symlink prop1 prop2 *inhib-weight*)
  (setf (get prop1 'contradicts) (push prop2 (get prop1 'contradicts)))
  (setf (get prop2 'contradicts) (push prop1 (get prop2 'contradicts)))
  (push (list prop1 prop2) *contradictions*)
  )

; **************************************************************
; CONTRADICT-SET handles cases where one set of propositions is
; contradictory with another set.

(defun contradict-set (props1 props2)
  (print-si props1 '" contradict " props2 '".")
  (push (list props1 props2) *contradictions*)
  (do ((props props1 (cdr props))
       (inhib-wt (/ *inhib-weight*
                    (/ (+ (length props1) (length props2)) 2)
                    )
                 )
       )
      ((null props) 'done)
    (make-inhib-links-for-unit (car props) props2 inhib-wt)
    )
  )



; ***************************************************************
; DATA notes that members of a list of propositions are evidence
; nodes, each of which is then linked to the special evidence unit.
; If a member of the list is a list, with the form
; (proposition importance), then the link to the special unit
; has weight importance*data-excit.
; Note: this should be called only after all the EXPLAIN
; statements, so that the check for unexplained evidence will work.

(defun data (list)
  (mapcar #'(lambda (unit-or-pair)
             (cond ((listp unit-or-pair)
                    (setf *all-data* (cons (car unit-or-pair) *all-data*))
                    (make-symlink 'special
                                   (car unit-or-pair)
                                   (* *data-excit* (second unit-or-pair))
                                   )
                    )
                   ; else:
                   (t (make-symlink 'special unit-or-pair *data-excit*)
                      (setf *all-data* (cons unit-or-pair *all-data*))
                      )
                   )
             )
          list
          )
  (if *check-unexplained* (unexplained)) ; check for unexplained data
  (print-si '"Data are: " *all-data*)
  )

; ADD-DATA: add data at a particular cycle. To work, this has to be
; called at each cycle, e.g. by debug-run.

(defun add-data (cycle evidence)
  (if (= (get *the-person* 'total-times) cycle)
      (data evidence)
    )
  )
; **************************************************************
; UNEXPLAINED check to see if there are any unexplained pieces of
; evidence, increasing decay if there are.
; NOTE: THIS IS BUGGY IF MORE THAN ONE DATA STATEMENT IS USED.

(defun unexplained ()
  (cond ((null *all-explainers*)
         (print-si '"ERROR: Do the EXPLAIN statements before the DATA statements.")
         )
        (t (print-si '"Unexplained data: "
                     (set-difference *all-data* *all-explained*)
                     )
           (setf *decay-register* *decay-amount*)
           (decay (* *decay-amount* (/ (length *all-data*)
                                       (length *all-explained*)
                                       )
                     )
                  )
           )
        )
  )

; ***************************************************************
; EXPLAIN sets up excitatory links noting what explains what.
; Weights depend on the number of explaining hypotheses.
; Value should be between 0 and 1.

(defun explain (list-of-explainers explanandum &optional (value 1))
  (let ((simpl-wt
         (cond ( (or (zerop *simpl-impact*)
                     (null (set-difference list-of-explainers *all-data*))
                     )
                (* *excit-weight* value)
                )
               ; otherwise:
               (t (/ (* *excit-weight* value)
                     (expt (length (set-difference list-of-explainers *all-data*))
                           *simpl-impact*
                           )
                     )
                  )
               )
         ))
    (print-si " " list-of-explainers
              (if (= (length list-of-explainers) 1) " explains " " explain ")
              explanandum
              )
    (setf *all-explainers* (union list-of-explainers *all-explainers*))
    (pushnew explanandum *all-explained*)
    (mapcar 'make-explan-unit (cons explanandum list-of-explainers))
    (if *echo2-mode*
        (setf (get explanandum 'explained-by)
              (union list-of-explainers
                     (get explanandum 'explained-by)
                     )
              )
      (setf (get explanandum 'explainers)
            (union list-of-explainers
                   (get explanandum 'explainers)
                   )
            )
      )
    ; record what explains what
    (note-co-hyps list-of-explainers explanandum)
    ; excitatory links between explanandum and explainers:
    (make-excit-links explanandum list-of-explainers simpl-wt)
    
    ; excitatory links among co-hypotheses, except data
    (make-all-excit-links (set-difference list-of-explainers *all-data*)
                          (* simpl-wt *co-hyp-importance*)
                          )
    )
  )

; ADD-EXPLAIN

(defun add-explain (cycle explainers explained)
  (if (= (get *the-person* 'total-times) cycle)
      (explain explainers explained)
    )
  )
; **********************************************************
; MAKE-ALL-EXCIT-LINKS sets up excitatory links between every
; pair of a set of units.

(defun make-all-excit-links (list-of-units weight)
  (do ((units list-of-units (cdr units)))
      ; exit:
    ((null (cdr units)) (get *the-person* 'total-links))
    
    ; repeat:
    (make-excit-links (car units)
                      (cdr units)
                      weight)))

; **********************************************************
; MAKE-EXCIT-LINKS sets up excitatory links between a unit and
; each of a set of units.

(defun make-excit-links (unit other-units weight)
  (do ((units other-units (cdr units)))
      ((null units) (return 'done))
    ; repeat:
    (make-symlink unit (car units) weight)))

; **********************************************************
; MAKE-INHIB-LINKS sets up inhibitory links among all pairs of
; a set of units.

;; *THE-PERSON* MUST BE SET CORRECTLY
(defun make-inhib-links (units weight)
  (do ( (unts (remove nil units) (cdr unts))
       (units-without-nils (remove nil units)))
      ; exit:
    ((null (cdr unts)) (get *the-person* 'total-links))
    ; repeat:
    (make-inhib-links-for-unit (car unts)
                               units-without-nils
                               weight)))

; **********************************************************
; MAKE-INHIB-LINKS-FOR-UNIT does it for one unit.

;; *THE-PERSON* MUST BE SET CORRECTLY
(defun make-inhib-links-for-unit (unit units weight)
  (do ( (unts units (cdr unts)))
      ; exit:
    ( (null unts) (get *the-person* 'total-links))
    ; action:
    (make-symlink unit (car unts) weight)))




; ********************************************************************
; ANALOGOUS sets up links between 2 analogous hypotheses and
; between 2 analogous pieces of evidence.

(defun analogous (hyps data)
  (cond ((and (member (car data) (get (car hyps) 'explains))
              (member (second data) (get (second hyps) 'explains))
              )
         ; then:
         (make-symlink (car hyps) (second hyps)
                        (* *excit-weight* *analogy-impact*)
                        )
         (make-symlink (car data) (second data)
                        (* *excit-weight* *analogy-impact*)
                        )
         (print-si '"Explanation of " (car data)
                   '" by " (car hyps)
                   '" is analogous to explanation of " (second data)
                   '" by " (second hyps)
                   )
         )
        ; else:
        (t (print-si '"Error: non-explanatory analogy."))
        )
  )



; *********************************************************************
; MAKE-EXPLAN-UNIT sets up a unit for use by network.

(defun make-explan-unit (name)
  (unless (member name (get *the-person* 'all-units)) (note-unit name)))

; ******************************************************************
; GET-EXPLAINERS

(defun get-explainers (datum)
  (get datum (if *echo2-mode* 'explained-by 'explainers)))

; ******************************************************************
; NOTE-CO-HYPS notes the co-hypotheses and explananda of each hypothesis.

(defun note-co-hyps (explainers explained)
  (if *echo2-mode* (setf (get explained 'explained-by)
                         (union (get explained 'explained-by) explainers)
                         )
    )
  (do ((hyps explainers (cdr hyps)))
      ; exit
    ((null hyps) 'done.)
    ; repeat:
    (setf (get (car hyps) 'explains)
          (cons explained (get (car hyps) 'explains))
          )
    (setf (get (car hyps) 'co-hypotheses)
          (union (get (car hyps) 'co-hypotheses)
                 (remove (car hyps) explainers)
                 )
          )
    (setf (get (car hyps) 'explanations)
          (cons (list explainers explained)
                (get (car hyps) 'explanations)
                )
          )
    )
  )

; ********************************************************

; Functions to count links made: START-EXPLAIN, STOP-EXPLAIN
; START-CONTRA, STOP-CONTRA

(defun start-explain ()
  (setf *start-links* (symlinks))
  )

(defun stop-explain ()
  (print-si "Symmetric excitatory links created for explanations: "
            (- (symlinks) *start-links*)
            )
  )

(defun start-contra ()
  (setf *start-links* (symlinks))
  )

(defun stop-contra ()
  (print-si "Symmetric inhibitory links created for contradictions: "
            (- (symlinks) *start-links*)
            )
  )

; SYMLINKS
(defun symlinks ()
  (/ (get *the-person* 'total-links) 2)
  )



; ********************************************************
; Noting competitors.
; *********************************************************
;
; MAKE-COMPETITION

(defun make-competition ()
  (setf *start-links* (symlinks))
  (print-si "Looking for competing hypotheses ...")
  (competing (find-competitors))
  (print-si "Symmetric inhibitory links created for competition: "
            (- (symlinks) *start-links*)
            )
  )

; FIND-COMPETITORS returns a list of pairs of propositions that
; are incoherent because they compete. Duplication is avoided:
; the returned list does not have both (A B) and (B A).

(defun find-competitors ()
  (do ((exp-list *all-explained* (cdr exp-list))
       (result nil)
       )
      ; exit
    ((null exp-list) result)
    ; repeat
    (setf result (union result
                        (competing-exp (car exp-list))
                        :test #'duplicate
                        )
          )
    )
  )







; ************************************************************

; EXP-RELEVANT determines if two propositions are explanatorily relevant
; to each other: one explains the other, or together they explain
; something, or they have a common explanation. Also spots
; pairs where one is a fact or piece of evidence, if EVID-COMPET is T.

(defvar evid-compet? nil)

(defun exp-relevant (prop1 prop2)
  (or (member prop1 (get prop2 'explains))
      (member prop2 (get prop1 'explains))
      (member prop1 (get prop2 'co-hypotheses))
      (intersection (get-explainers prop1) (get-explainers prop2))
      (and evid-compet? (member prop1 *all-data*))
      (and evid-compet? (member prop2 *all-data*))
      )
  )

; *************************************************************

; COMPETING-EXP puts together pairs of competing
; hypotheses. It screens out ones that are
; not competing because they are explanatorily relevant to each other.

(defun competing-exp (explained)
  (do ((explainers (set-difference (get explained 'explained-by)
                                   *common-hyps*
                                   )
                   (cdr explainers)
                   )
       (result nil)
       )
      ; exit
    ((null (cdr explainers)) result)
    ; repeat
    (setf result (union-dup result
                            (pair-up-comp (car explainers) (cdr explainers))
                            )
          )
    
    )
  )



; ************************************************************
; UNION-DUP

(defun union-dup (lst1 lst2)
  (union lst1 lst2 :test #'duplicate)
  )

; DUPLICATE

(defun duplicate (list1 list2)
  (or (equal list1 list2)
      (equal (reverse list1) list2)
      )
  )

; ************************************************************
; PAIR-UP-COMP creates a list of pairs consisting of an element
; and each member of a list, screening out non-competitors.

(defun pair-up-comp (element lst)
  (do ((ls lst (cdr ls))
       (result nil)
       )
      ;exit
    ((null ls) result)
    ;repeat
    (unless (exp-relevant element (car ls))
      (setf result (cons (list element (car ls)) result))
      )
    )
  )



; ****************************************************
; COMPET-NOT-CONTRA returns a list of propositions found
; to be competitive but not explicitly contradictory.

(defun cnc () (compet-not-contra))

(defun compet-not-contra ()
  (print-si "The competing but not contradictory propositions are:")
  (print-si " "
            (set-difference *competitors* *contradictions* :test #'equal)
            )
  )

; ****************************************************

; COMPETING sets up inhibitory links between competing propositions:

(defun competing (pairs-of-competitors)
  (setf *competitors* nil)
  (do ((prs (sort pairs-of-competitors #'string-lessp :key #'car) (cdr prs))
       prop1 prop2
       )
      ; exit
    ((null prs) 'done)
    ; repeat:
    (setf prop1 (first (car prs)))
    (setf prop2 (second (car prs)))
    (print-si prop1 " competes with " prop2 " because of "
              (intersection (get prop1 'explains) (get prop2 'explains)) "."
              )
    (make-compet-link prop1 prop2)
    (setf (get prop1 'competes-with) (pushnew prop2 (get prop1 'competes-with)))
    (setf (get prop2 'competes-with) (pushnew prop1 (get prop2 'competes-with)))
    (pushnew (list prop1 prop2) *competitors*)
    )
  )
; *********************************************************
; MAKE-COMPET-LINK sets up an inhibitory link between units representing
; propositions that incohere because they compete to explain some
; pieces of evidence. Links are weaker in proportion to the
; number of cohypotheses involved in the explanations that produce
; the competition. Links are stronger the more pieces of evidence
; competed for. Note that unlike make-symlink this allows
; summation of inhibition. If two propositions are both
; contradictory and competitive, inhibition will be all the greater.
; This is a change of July 9, 1990. (put in Allegro ECHO 7/11/90)
; UPDATE 6-21-2000, PT. Based on the OJ simulation, I decided
; that the inhibitory links resulting from summing competition
; might be too high. So I've tried replacing the old make-compet-link
; with a simpler one.

(defun make-compet-link (pro1 pro2)
  (make-link pro1 pro2 *inhib-weight*)
  (make-link pro2 pro1 *inhib-weight*)
  )



(defun make-compet-link-old (pro1 pro2)
  (let ((wt (* *inhib-weight*
               (/ (length (intersection (get pro1 'explains)
                                        (get pro2 'explains)
                                        )
                          )
                  (/ (length (co-hyps-used pro1 pro2))
                     2
                     )
                  )
               )
            )
        )
    (make-link pro1 pro2 wt)
    (make-link pro2 pro1 wt)
    )
  )





; CO-HYPS-USED

(defun co-hyps-used (pr1 pr2)
  (do ((evid (intersection (get pr1 'explains)
                           (get pr2 'explains)
                           ) (cdr evid)
             )
       (result nil)
       )
      ((null evid) result)
    ;
    (setf result (union result
                        (union (co-hyps-in-expln pr1 (car evid))
                               (co-hyps-in-expln pr2 (car evid))
                               )
                        )
          )
    )
  )





; ADD-TO-END

(defun add-to-end (atm lst)
  (setf lst (reverse (cons atm (reverse lst))))
  )
; *********************************************************
; MEAN-ACTIVATION

(defun mean-activation (lst)
  (/ (apply #'+ (mapcar #'get-activation lst))
     (length lst)
     )
  )

;(defun get-activation (unit) (get unit 'activation))

; *********************************************************
; Running the network.
; **********************************************************
; RUN-ECHO runs the network.

(defun run-echo ()
  "Checks to see which type of run to do, calls apropriate run routine."
  (if *echo2-mode* (run-echo2-exp) (run-echo-exp))
  )

(defun run-echo2-exp ()
  (print-si) ; line-feed
  (print-si "Running ECHO.2.")
  (setf *acme-mode* nil)
  (setf (get 'special 'activation) *special-activation*)
  (make-competition)
  (run-hyp-net)
  (if (and *check-unexplained*
           (/= *decay-amount* *decay-register*) ; because of unexplained
           )
      (decay *decay-register*)
    )
  (unless *silent-run?* (print-propns))
  )

(defun run-echo-exp ()
  (print-si) ; line-feed
  (print-si "Running ECHO.")
  (setf *acme-mode* nil)
  (note-explanations)
  ; help higher level explainers by creating links from them
  ; down to data
  (run-hyp-net)
  (if (and *check-unexplained*
           (/= *decay-amount* *decay-register*) ; because of unexplained
           )
      (decay *decay-register*)
    )
  (print-propns)
  )







; **********************************************************
; LAST-EL

(defun last-el (lst)
  (car (last lst))
  )
; **********************************************************
; CO-HYPS-IN-EXPLN says what hypotheses were used in the
; explanation of Q by P. It can handle cases where P plays a
; role in explaining Q by different other hypotheses.
;

(defun co-hyps-in-expln (unit1 unit2)
  (do ((explns (get unit1 'explanations) (cdr explns))
       (result nil)
       )
      ((null explns) result)
    ;
    (if (equal unit2 (last-el (car explns)))
        (setf result
              (union result
                     (caar explns)
                     )
              )
      )
    )
  )

; **********************************************************

; NOTE-EXPLANATIONS
(defun note-explanations ()
  (setf (get 'special 'activation) *special-activation*)
  
  )





; **********************************************************
; Assessing the results:
; See MacECHO
; **********************************************************







; END OF echo.lisp
