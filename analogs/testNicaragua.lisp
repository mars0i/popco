; testNicaragua.lisp

;; NOTE:
;; In this scenario, the target and source analog structures contain
;; exactly the same predicates, so the only new concept that could be added
;; when a proposition is added to a structure is an object-concept.

; Uses Nicaragua/Contras example from Holyoak and Thagard 1989
; "Analog Retrieval by Constraint Satisfaction".

;; first clear everything out
(mapcar #'clear-plists (get 'politicians 'members))
(clear-person-nets 'politicians)

;; POLITICIANS:
;; some are set up to map Contras to Hungarian freedom-fighters;
;; some are set up to map Contras to PLO terrorists;
;; and some don't know--they are lacking any source propositions (will that work?)
;; How will communication affect them?
(format t "Making politicians...~%")

(defvar pols 'politicians)

; Pauly thinks the Hungarian situation is a better analogy
(make-person 'pauly 'politicians '()
		; the target analog structure
             '((make-struc 'target 'problem ; "target" is hardcoded elsewhere, so is required.
                           '(start          ; I don't remember whether "start" and "problem" are required by PT's code.
                              ((country (Nicaragua) c1) 
                               (govern (Sandinistas Nicaragua) c2)          ; syntax: predicate, arguments, proposition name
                               (aim-to-overthrow (Contras Sandinistas) c3) 
                               (terrorists? (Contras Contra-terror?) c4) 
                               (freedom-fighters? (Contras Contra-freedom?) c5) 
                               (should-support? (US Contras support?) c6))))
		; the source analog structure
               (make-struc 'source 'problem
                           '(start 
                              ((country (Hungary) h1) 
                               (govern (communists Hungary) h2) 
                               (aim-to-overthrow (Hungarians communists) h3) 
                               (terrorists? (Hungarians terrorists-no) h4) 
                               (freedom-fighters? (Hungarians freedom-fighters-yes) h5) 
                               (should-support? (US Hungarians support-yes) h6))))))

; Polly thinks the Israeli/Palestinian situation is a better analogy
(make-person 'polly 'politicians '()
             '((make-struc 'target 'problem
                           '(start 
                              ((country (Nicaragua) c1) 
                               (govern (Sandinistas Nicaragua) c2) 
                               (aim-to-overthrow (Contras Sandinistas) c3) 
                               (terrorists? (Contras Contra-terror?) c4) 
                               (freedom-fighters? (Contras Contra-freedom?) c5) 
                               (should-support? (US Contras support?) c6))))
               (make-struc 'source 'problem
                           '(start 
                              ((country (Israel) i1) 
                               (govern (Israelis Israel) i2) 
                               (aim-to-overthrow (PLO Israelis) i3) 
                               (terrorists? (PLO terrorists-yes) i4) 
                               (freedom-fighters? (PLO freedom-fighters-no) i5) 
                               (should-support? (US PLO support-no) i6))))))

; Pawley has no idea what's a good analogy for the Nicaraguan situation
(make-person 'pawley 'politicians '()
             '((make-struc 'target 'problem
                           '(start 
                              ((country (Nicaragua) c1) 
                               (govern (Sandinistas Nicaragua) c2) 
                               (aim-to-overthrow (Contras Sandinistas) c3) 
                               (terrorists? (Contras Contra-terror?) c4) 
                               (freedom-fighters? (Contras Contra-freedom?) c5) 
                               (should-support? (US Contras support?) c6))))
               (make-struc 'source 'problem
                           '(start 
                              ()))))

(n-persons 'pawley 20 0) ; make a bunch of persons who are just like Pawley

(create-nets 'politicians) ; this step is required
(format t "Politicians made: ")
(my-print (get 'politicians 'members))
