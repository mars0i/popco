;; crime3.lisp
;; Wraper file for Thibodeau/Boroditsky-inspired crime is a virus/beast analogies.
;; The proposition and semantic specs are in crime3propns.lisp, which is loaded
;; by this file.
;; SEE crime3withnotes.lisp for notes that used to be here.
;; Also see crime2.lisp for other notes, old commented out code not present here, etc..

(load "crime/crime3propns")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *propn-category-prefixes* '("CV" "CB" "V" "B"))
(setf *propn-category-descriptions* '("virus-like crime propns" "beast-like crime propns" "virus propns" "beast propns")) ; these should match

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persons who only talk about crime:

(defun make-no-bias-crime-talker (name &optional (given '()) (groups 'folks) (talks-to '()) (num-listeners 1))
    (make-person name groups given
                 `((make-struc 'target 'problem '(start (,@crime-propns)))
                   (make-struc 'source 'problem '(start ()))
                   ,@semantic-relations)
                 '()          ; addl-input
                 '(target)    ; converse-strucs
                 talks-to
                 num-listeners))

(defun make-virus-bias-crime-talker (name &optional (given '()) (groups 'folks) (talks-to '()) (num-listeners 1))
    (make-person name groups given
                 `((make-struc 'target 'problem '(start (,@crime-propns)))
                   (make-struc 'source 'problem '(start (,@virus-propns)))
                   ,@semantic-relations)
                 '()          ; addl-input
                 '(target)    ; converse-strucs
                 talks-to
                 num-listeners))

(defun make-beast-bias-crime-talker (name &optional (given '()) (groups 'folks) (talks-to '()) (num-listeners 1))
    (make-person name groups given
                 `((make-struc 'target 'problem '(start (,@crime-propns)))
                   (make-struc 'source 'problem '(start (,@beast-propns)))
                   ,@semantic-relations)
                 '()          ; addl-input
                 '(target)    ; converse-strucs
                 talks-to
                 num-listeners))

(defun make-both-bias-crime-talker (name &optional (given '()) (groups 'folks) (talks-to '()) (num-listeners 1))
    (make-person name groups given
                 `((make-struc 'target 'problem '(start (,@crime-propns)))
                   (make-struc 'source 'problem '(start (,@virus-propns ,@beast-propns)))
                   ,@semantic-relations)
                 '()          ; addl-input
                 '(target)    ; converse-strucs
                 talks-to
                 num-listeners))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persons who talk about everything:

(defun make-virus-bias-all-talker (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start (,@crime-propns)))
                   (make-struc 'source 'problem '(start (,@virus-propns)))
                   ,@semantic-relations)
                 '()
                 '()))

(defun make-beast-bias-all-talker (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start (,@crime-propns)))
                   (make-struc 'source 'problem '(start (,@beast-propns)))
                   ,@semantic-relations)
                 '()
                 '()))

(defun make-both-bias-all-talker (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start (,@crime-propns)))
                   (make-struc 'source 'problem '(start (,@virus-propns ,@beast-propns)))
                   ,@semantic-relations)
                 '()
                 '()))

(defun make-no-bias-all-talker (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start (,@crime-propns)))
                   (make-struc 'source 'problem '(start ()))
                   ,@semantic-relations)
                 '()
                 '()))

(defun make-naive-all-talker (name &optional (given '()))
    (make-person name 'folks given
                 `((make-struc 'target 'problem '(start ()))
                   (make-struc 'source 'problem '(start ()))
                   ,@semantic-relations)
                 '()
                 '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; leave these at defaults:
; *propn-excit-weight* *propn-inhib-weight* *trust* *perceived-excit*
;(setf *time-runs* nil)
;(setf *do-converse* t)  ; set this in particular model files
(setf *do-update-propn-nets* t)
(setf *do-report-to-netlogo* t)
(setf *do-report-propns-to-csv* t)
(setf *do-report-analogy-nets-to-guess* t)
(setf *sleep-delay* nil)           ; If non-nil, pause this many seconds between generations
(setf *silent-run?* t)             ; If nil, use Thagard-style verbose reporting to console

;; first clear everything out
(mapcar #'clear-plists (get 'folks 'members))
(clear-person-nets 'folks)
;(kill-everyone)
(setf *the-population* 'folks)
