;; popco-fmt-netlogo.lisp
;; Copyright (c) 2012 by Marshall Abrams
;; May be distributed only with permission from the author.
;;
;; Data-formatting functions for output to NetLogo
;; Copyright (c) 2011 by Marshall Abrams
;; May be distributed only with permission from the author.

;;----------------------------------------------------------
;; POPCO/COHERE->NETLOGO functions

(defvar *netlogo-syntax-description*
";SYNTAX: 
; list-of-proposition-category-prefixes list-of-proposition-category-descriptions 
; initial-list-of-persons 
; list-of-persons list-of-conversations list-of-persons list-of-conversations ...
; Where list-of-persons is optionally preceded by a list consisting of the status-message flag
; and a status message string, and where:
; person is: [name [proposition ...] [predicate ...] [object ...] [maplink ...] [propn-activation ...]
; maplink is: [proposition proposition activation]
; propn-activation is: activation of the proposition at the corresponding index in 
; the earlier proposition list in the same person.
; conversation: [is-new-to-listener proposition speaker listener]
; [Note: A maplink is a representation of an ACME map unit.  The activation in the maplink 
; triple is the activation of that unit.  However, weights of links in the proposition net
; are a (probably nonlinear) function of activations of corresponding proposition 
; map units, so a maplink activation can function as a very crude representation of 
; a proposition link weight.]

" )

;; FMT-PERSON-FOR-NETLOGO
;; NOTE: This implements the data structure which is sent to NetLogo.
;; THE ORDER OF ITEMS IS FIXED and must be kept in sync with the NetLogo code.
;; NOTE *netlogo-syntax-description* above, which is prepended to output file going to NetLogo.
(defun fmt-person-for-netlogo (person)
  (setf *the-person* person)
  (list person
        (mapcar #'personal-to-generic-sym (get person 'all-propositions)) ; propn names
        '() ;(mapcar #'personal-to-generic-sym (get person 'all-preds))        ; predicate names  ; NOT CURRENTLY USED
        '() ;(mapcar #'personal-to-generic-sym (get person 'all-objects))      ; object names     ; NOT CURRENTLY USED
        (mapcar #'fmt-map-unit-for-netlogo (remove-if-not #'propn-map? (get person 'all-units))) ; use #'is-acme-unit to get all map units
        (mapcar #'fmt-activation (get person 'all-propositions)))) ; activactions of propns, in the same order as propn names

(defun fmt-map-unit-for-netlogo (unit)
  (append (mapcar #'personal-to-generic-sym (get unit 'concerns))
          (list (coerce (get unit 'activation) 'single-float))))

(defun fmt-activation (unit)
  (if (activation unit)
    (coerce (activation unit) 'short-float)
    0.0))

;; Replace fmt-list-string with fmt-tree-..., as in old report-activation?
;; [No: Then can't take advantage of some Lisp's default pretty-printing.]
(defun fmt-conversations-for-netlogo (conversations)
  (fmt-list-string-for-netlogo
    (prin1-to-string 
      (mapcar #'fmt-conversation-for-netlogo conversations))))

;; NOTE *netlogo-syntax-description* above, which is prepended to output file going to NetLogo.
(defun fmt-conversation-for-netlogo (conversation)
  (cons (fmt-new-flag-for-netlogo (car conversation))
        (mapcar #'symbol-name (cdr conversation))))

(defun fmt-new-flag-for-netlogo (flag)
  (cond ((null flag) 0)
        ((eq t flag) 1)
        (t (error "FORMAT-NEW-FLAG-FOR-NETLOGO: ~s is neither t nor nil." flag))))

;; FORMAT-LIST-STRING-FOR-NETLOGO
;; Given a string that's a printable representation of a Lisp list, replaces
;; all parentheses with square brackets (NetLogo's list delimiter).
(defun fmt-list-string-for-netlogo (listring)
  (substitute #\] #\) (substitute #\[ #\( listring)))   ; faster to explode string and do both at once? use a regexp lib?

;; FMT-TREE-FOR-NET-LOGO
;; Given a list--possibly of lists [of lists, etc.]--
;; produces a string which is a representation of the same list [of lists, etc.],
;; but with square brackets instead of parentheses.
(defun fmt-tree-for-netlogo (tree)
  (let ((netlogo-strings 
          (flatten (fmt-tree-for-netlogo-aux tree)))) ; flatten is in popco-utils.lisp
    (if (atom netlogo-strings)
      netlogo-strings
      (apply #'concatenate (cons 'string netlogo-strings)))))

; Rather than just substituting square brackets for parens, we need a more rococo strategy
; when the list we're formatting can contain empty lists, which Lisp will print as "NIL" not "()".
; Can't just substitute "[]" for "NIL" since some symbol might contain "nil", and parsing the
; string isn't worth the trouble given that Lisp does that for us. (Are there portable internal READ functions?)
(defun fmt-tree-for-netlogo-aux (tree)
  (cond ((null tree) " [] ")
        ((symbolp tree) (concatenate 'string " \"" (symbol-name tree) "\" "))
        ((stringp tree) (concatenate 'string " \"" tree "\" "))
        ((atom tree)    (concatenate 'string " "   (princ-to-string tree) " "))
        (t (list " [" 
                 (mapcar #'fmt-tree-for-netlogo-aux tree)
                 "] "))))

(defun format-netlogo-status-message ()
  (format nil "[\"~A\" \"~A\"]~%" *netlogo-status-flag* *netlogo-status-message*))
