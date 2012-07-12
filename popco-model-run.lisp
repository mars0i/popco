;; popco-model-run.lisp
;; Copyright (c) 2012 by Marshall Abrams
;; May be distributed only with permission from the author.
;;
;; Useful tools for for model runs





; MAKE-PERSONS-WITH-ADDL-PROPN 
; Make as many persons as there are propns in addl-target-propns, adding one such propn to each person.
; Persons will be named basename+incremented-number.
; Arguments:
;       person-maker: function that will create a person from a person name and an additional 
;                     proposition to be added to whatever the function puts in target analog struc by default.
;       basename: base name of persons to be made
;       addl-target-propns: a list of additional propositions to be added to each new person's target analog struc
;       first-number [optional--defaults to 1]: start of numbers to add to basename
;
; Example usage:
;       (make-persons-with-addl-propn #'make-skyless-person 'e sky-origin-propns)
;       (make-persons-with-addl-propn #'make-earthless-person 's earth-origin-propns)
;
(defun make-persons-with-addl-propn (person-maker basename addl-target-propns &optional (first-number 1))
  (let ((names 
          (make-names2 basename first-number (length addl-target-propns)))) ; make one person name for each addl propn to be added
    (mapc person-maker names addl-target-propns)))



; POP-HAS-MEMBER-WITH-THESE-PROPNS-IN-STRUC? 
; Test for the existence of a person with all members of a set of propns.
; Note: Propositions are proposition symbols, NOT the messages containing predicate, arguments, and name.
(defun find-member-with-propns-in-struc? (generic-struc propns &optional (population *the-population*))
  (find-if #'(lambda (pers) (person-struc-has-these-propns? pers generic-struc propns))
           (get population 'members)))

(defun person-struc-has-these-propns? (person generic-struc propns)
  (struc-has-these-propns? (generic-to-personal-sym generic-struc person)
                           (mapcar #'(lambda (p) (generic-to-personal-sym p person))
                                   propns)))

(defun struc-has-these-propns? (personal-struc propns)
  (subsetp propns (get personal-struc 'propositions)))

