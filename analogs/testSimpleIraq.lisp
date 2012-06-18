; testSimpleIraq.lisp

;; first clear everything out
(cond ((get 'bushes 'members)
       (format t "clearing old stuff out...~%")
       (clear-person-nets 'bushes)
       (mapcar #'clear-props (get 'bushes 'members))))

(format t "~%Making bushes...~%")

; bushA understands simple Iraq-wwII analogy [pp. 248ff Holyoak & Thagard 1995]
(make-person 'bushA 'bushes
             '()
             '((make-struc 'target 'problem
                              '(start
                                 ((president-of (Saddam Iraq) PSI)
                                  (invade (Iraq Kuwait) IIK))))
               (make-struc 'source 'problem
                           '(start
                              ((fuhrer-of (Adolph Germany) FHG)
                               (occupy (Germany Austria) OGA))))))

; bushB is ignorant of the analogy, at first
(make-person 'bushB 'bushes
             '()
             '((make-struc 'target 'problem
                              '(start
                                 ((president-of (Saddam Iraq) PSI)
                                  (invade (Iraq Kuwait) IIK))))
               (make-struc 'source 'problem
                           '(start
                              ()))))

;(n-persons 'bushA 2 0)
;(n-persons 'bushB 2 0)
(n-persons 'bushA 10 0)
(n-persons 'bushB 10 0)

(create-nets 'bushes)
(format t "bushes made: ")
(my-print (get 'bushes 'members))

; abbrevs
(setf ba 'bushA)
(setf bb 'bushB)
(setf bs 'bushes)

; These give an alternative structure, for testing the effect
; of adding propositions while popco is running.

; Call e.g. (add-struc 'bushsource 'start `(,tismsg)) and then do something to
; modify the person bushA and then (create-net 'bushA) to add
; this proposition to bushA's network:
(setf tismsg '(threaten (italy switzerland) TIS))
