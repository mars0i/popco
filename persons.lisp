;; FILE: persons.lisp
;; Functions for creating persons
;; Based originally on parts of Paul Thagard's consensus.lisp (CREATED: September 17, 1998)
;; consensus.lisp was modified by Marshall Abrams, 2011-2012.
;; Then 2/2013 replaced make-person and persons-like with the new versions
;; that Kristen Hammack wrote in 12/2012 and 1/2013.  These were originally
;; in social-net-functions.lisp.

; MAKE-PERSON sets up the input information for each person.
; GROUP is the consensus group to which the person belongs.
; GIVEN lists the favored information - data for ECHO, goals 
; [GIVEN propns are in full subj/pred message format--not just the propn name symbol]
; for DECO, observed for IMP ...
; and perceived for POPCO. [MA 11/2011]
; INPUT is the functions from ECHO etc. for creating a network
; for the person. Other properties added later: ACCEPTED, REJECTED.
; MA 12/2011:
; Added addl-input so that calls ot e.g. IMPORTANT can be put there,
; and called later *after* the net is created by constraint-map from
; basic data created by code in input.  IMPORTANT needs this to happen
; first so that the constraint-hyps fields, which it uses, get filled.
; PRAGMATIC calls can also go there, but semantic calls such as SIMILAR
; must go in initial-input.
; 4/12/2012 added converse-strucs: list of analog structs from which propns can be uttered.
; If nil, then all of them.
; [To cause silence in a conversing population, list dummy struc name.]
; SUMMARY OF USUAL USAGE:
; (make-person 'name-of-person *the-population* propns-to-be-perceived-initially  ; NOTE PROPNS ARE IN FULL SUBJ/PRED MESSAGE FORMAT
;              '((make-struc 'target 'problem '(start list-of-target-propns))
;                (make-struc 'source 'problem '(start list-of-source-propns))
;                semantic-specification-1
;                semantic-specification-2
;                etc.)
;              list-of-pragmatic-relations-if-any
;              list-of-analog-strucs-from-which-to-utter-or-nil-for-all)
; OR:
; (make-person 'name-of-person *the-population* propns-to-be-perceived-initially
;              `((make-struc 'target 'problem '(start (,@propns-to-subst-in-here)))
;                (make-struc 'source 'problem '(start (,@propns-to-subst-in-here)))
;                ,@semantic-specs-to-subst-in-here)
;              `(@,pragmatic-relations)
;              '()) ; put 'source or 'target in list to restrict utterances to propns in that struc
;
(defun make-person (person group given initial-input &optional addl-input converse-strucs)
  (initialize-person-properties person)  ; From popco.lisp. Note: setfs *the-person* to person
  (put person 'group group)
  (put group 'members (cons-if-new person (get group 'members))) ; changed push to cons-if-new  -MA 6/2011
  (put person 'given-el given) ; NOTE PROPNS ARE IN FULL SUBJ/PRED MESSAGE FORMAT
  ; (put person 'other-el other)
  (put person 'input initial-input)
  (when addl-input 
    (put person 'addl-input addl-input))
  (put person 'converse-strucs (mapcar #'generic-to-personal-sym converse-strucs))
  person)
  ; the person's env will get added later

; PERSONS-LIKE creates a bunch of people like the given person
; This could alternatively use setf symbol-plist
(defun persons-like (old list-of-new)
  (do ((persons list-of-new (cdr persons))
       (result nil))
    ((null persons) result) ; repeat
    (push
      (make-person (car persons)
                   (get old 'group)
                   (get old 'given-el)
                   (get old 'input)
                   (get old 'addl-input)
                   (mapcar #'(lambda (personal-struc) (personal-to-generic-sym personal-struc old))
                           (get old 'converse-strucs)))
      result)))

; N-PERSONS makes num number of persons like a given one
; numbered starting with start.
(defun n-persons (name num start)
  (persons-like name (make-names name num start)))

; Similar but you can specify the new base name to be different,
; and always starts from 1, and uses make-names2 rather than make-names.
(defun n-persons-with-name (old-person new-basename number-to-create)
  (persons-like old-person (make-names2 new-basename 1 number-to-create)))

;; MAKE-NAMES makes a list of num names numbered starting with start.

(defun make-names (seed num start)
  (do ((n num (1- n))
       (result nil)
       (index start (1+ index)))
      ((= n 0) result)
    (push (simple-catname seed index) result))) ; changed to simple-catname -MA 7/2011


; new version of make-names that always formats numbers as 2-digit
(defun make-names2 (basename first-number number-to-make)
  (mapcar #'read-from-string 
          (make-names2-aux basename first-number number-to-make (decimal-digits number-to-make))))

(defun make-names2-aux (basename first-number number-to-make digits)
  (if (<= number-to-make 0)
    '()
    (cons (format nil "~A~2,'0d" basename first-number)
          (make-names2-aux basename (1+ first-number) (1- number-to-make) digits))))

; returns the number of decimal digits in an integer
(defun decimal-digits (int)
  (if (< int 1)
    0
    (+ 1 (decimal-digits (/ int 10)))))

