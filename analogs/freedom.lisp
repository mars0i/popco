; freedom.lisp

; an argument:
; even on less cynical views, it requires some doing to argue that
; iraq and afghanistan are about freedom.  at best they're about
; safety from islamic terrorists.  but fighting for freedom is a
; traditional justification, and is something felt to have 
; sufficient magnitude to justify loss of many american lives.
; however, I think that just an arbitrary mapping between freedom
; and something else won't be accepted.  you can't just paste anything
; in for freedom.  so there must be some similarity--some analogy.

(mapcar #'clear-plists (get 'americans 'members))
(clear-person-nets 'americans)

(make-person 'joe 'americans '()
             '((make-struc 'target 'problem
                           '(start
                              ((sacrifice (soldiers) t-sac-solds)
                               (protect (safety soldiers citizens) t-prot-safe)
                               (lose (safety citizens) unknown t-lose-safe)
                               (bad (t-lose-safe) t-bad-lose-safe)
			       ))) 
               (make-struc 'source 'problem 
                           '(start 
                              ((sacrifice (soldiers) s-sac-solds) ; SPECIAL this to false in target (or reconfigure)
                               (protect (freedom soldiers citizens) s-prot-freed)
                               (protect (safety soldiers citizens) s-prot-safe)
			       (justifies (s-prot-free s-sac-solds) s-free-justif-sac)  ; make this SPECIAL
			       (doesnt-justify (s-prot-safe s-sac-solds) s-safe-justif-sac)
                               (bad (s-lose-safety) s-bad-lose-safety) ; is the proposition argument somewhere?
                               (bad (s-lose-free) s-bad-lose-free) ; is the proposition argument somewhere?
			       ; obvious conclusions:
			       (fight-for-in (safety US Iraq) s-safety-iraq)
			       (fight-for-in (safety US Afghanistan) s-safety-afgh)
			       ; domino conclusions:
			       (fight-for-in (freedom US Europe) s-freedom-europe)
			       (fight-for-in (freedom US VietNam) s-freedom-vietnam)
			       (fight-for-in (freedom US Korea) s-freedom-vietnam)
			       ; or put justification connected to these wars
			       )))))


(make-person 'jane 'americans '()
             '((make-struc 'target 'problem
                           '(start
                              ((sacrifice (soldiers) t-sac-solds)
                               (protect (safety soldiers citizens) t-prot-safe)
                               (lose (safety citizens) unknown t-lose-safe)
                               (bad (t-lose-safe) t-bad-lose-safe)))) 
               (make-struc 'source 'problem 
                           '(start 
                              ()))))

(create-nets 'americans)
