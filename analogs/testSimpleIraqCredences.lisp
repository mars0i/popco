; testSimpleIraqWithCredences.lisp
; experiments adding credences to propositions
; initial credences are the third element in the proposition lists.
; they will get ignored unless code is called to extract and use them.
; i.e. the normal analogy-construction code only refers to list elements
; using Lisp functions first, second, and last.

;; first clear everything out
(mapcar #'clear-plists (get 'wonks 'members))
(clear-person-nets 'wonks)

; wonkA understands simple Iraq-wwII analogy [pp. 248ff Holyoak & Thagard 1995]
(make-person 'wonkA 'wonks
             '()
             '((make-struc 'target 'problem
                              '(start
                                 ((president-of (Saddam Iraq) 0.75 PSI)
                                  (invade (Iraq Kuwait) -0.5 IIK))))
               (make-struc 'source 'problem
                           '(start
                              ((fuhrer-of (Adolph Germany) 1.0 FHG)
                               (occupy (Germany Austria) 1.0 OGA))))))

; wonkB is ignorant of the analogy, at first
(make-person 'wonkB 'wonks
             '()
             '((make-struc 'target 'problem
                              '(start
                                 ((president-of (Saddam Iraq) PSI)
                                  (invade (Iraq Kuwait) IIK))))
               (make-struc 'source 'problem
                           '(start
                              ()))))

(n-persons 'wonkA 10 0)
(n-persons 'wonkB 10 0)

(create-nets 'wonks)

; abbrevs
(defvar ws 'wonks)

(format t "~%~S~%" (get ws 'members))

; These give an alternative structure, for testing the effect
; of adding propositions while popco is running.

; Call e.g. (add-struc 'wonksource 'start `(,tismsg)) and then do something to
; modify the person wonkA and then (create-net 'wonkA) to add
; this proposition to wonkA's network:
;(setf tismsg '(threaten (italy switzerland) TIS))
