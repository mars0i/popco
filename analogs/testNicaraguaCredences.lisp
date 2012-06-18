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

; NOTE: Credences must be entered as floats [e.g. 0.0, not 0] or some implementations  [e.g. SBCL]
; will object.  This is because various functions in network.lisp specify that certan numbers must be
; floats, which can allow more efficient compilation.

; Pauly thinks the Hungarian situation is a better analogy
(make-person 'pauly 'politicians '()
		; the target analog structure
             '((make-struc 'target 'problem
                           '(start
                              ((country (Nicaragua) .9 c1) 
                               (govern (Sandinistas Nicaragua) .9 c2)
                               (aim-to-overthrow (Contras Sandinistas) .9 c3) 
                               (terrorists? (Contras Contra-terror?) 0.0 c4)                  ; question
                               (freedom-fighters? (Contras Contra-freedom?) 0.0 c5)           ; question
                               (should-support? (US Contras support?) 0.0 c6))))              ; question
               (make-struc 'source 'problem
                           '(start 
                              ((country (Hungary) .9 h1) 
                               (govern (communists Hungary) .9 h2) 
                               (aim-to-overthrow (Hungarians communists) .9 h3) 
                               (terrorists? (Hungarians terrorists-no) .9 h4)               ; answer
                               (freedom-fighters? (Hungarians freedom-fighters-yes) .9 h5)  ; answer
                               (should-support? (US Hungarians support-yes) .9 h6))))))     ; answer

; Polly thinks the Israeli/Palestinian situation is a better analogy
(make-person 'polly 'politicians '()
             '((make-struc 'target 'problem
                           '(start 
                              ((country (Nicaragua) .9 c1) 
                               (govern (Sandinistas Nicaragua) .9 c2) 
                               (aim-to-overthrow (Contras Sandinistas) .9 c3) 
                               (terrorists? (Contras Contra-terror?) 0.0 c4)                  ; question
                               (freedom-fighters? (Contras Contra-freedom?) 0.0 c5)           ; question
                               (should-support? (US Contras support?) 0.0 c6))))              ; question
               (make-struc 'source 'problem
                           '(start 
                              ((country (Israel) .9 i1) 
                               (govern (Israelis Israel) .9 i2) 
                               (aim-to-overthrow (PLO Israelis) .9 i3)                      ; answer
                               (terrorists? (PLO terrorists-yes) .9 i4)                     ; answer
                               (freedom-fighters? (PLO freedom-fighters-no) .9 i5)          ; answer
                               (should-support? (US PLO support-no) .9 i6))))))

; Pawley has no idea what's a good analogy for the Nicaraguan situation
(make-person 'pawley 'politicians '()
             '((make-struc 'target 'problem
                           '(start 
                              ((country (Nicaragua) 9. c1) 
                               (govern (Sandinistas Nicaragua) .9 c2) 
                               (aim-to-overthrow (Contras Sandinistas) .9 c3) 
                               (terrorists? (Contras Contra-terror?) 0.0 c4)                  ; question
                               (freedom-fighters? (Contras Contra-freedom?) 0.0 c5)           ; question
                               (should-support? (US Contras support?) 0.0 c6))))              ; question
               (make-struc 'source 'problem
                           '(start 
                              ()))))

(n-persons 'pawley 20 0) ; make a bunch of persons who are just like Pawley

(create-nets 'politicians) ; this step is required
(format t "Politicians made: ")
(my-print (get 'politicians 'members))

; Subpopulations of single individuals--useful for experimentation.
; e.g. you can run popco on one of these by itself, and you won't
; you can settle nets, etc. without adding propositions due to
; conversation.
(setf (get 'polly-pop 'members) '(polly))
(setf (get 'pauly-pop 'members) '(pauly))
(setf (get 'pawley-pop 'members) '(pawley))
