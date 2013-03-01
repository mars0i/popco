;;;; natsel.lisp
;;;; Authors: Kristen Hammack and Marshall Abrams
;;;;
;;;; Originally based on:
;;;; nat-selection1.lisp
;;;; sandbox for natural selection functionality in POPCO
;;;; Author:    Kristen Hammack
;;;; Vers:      1.0.0 08/2012 kmh - initial coding
;;;;            2.0.0 01/2013 kmh - deleted the model; just functions now.




(defun remove-from-group (person group)
  (setf (get group 'members) (remove person (get group 'members))))

;;;**********************
;;;DEATH AND BIRTH FUNCTIONS

;;Removes person from *the-population* and groups and uninterns the symbol for the person
(defun kill (person)
  (mapc #'(lambda (group) (remove-from-group person group)) *all-social-net-groups*)
  (remove-from-group person *the-population*)
  (unintern person))

;;Does what it says.  Calls death on every person in the given population.
(defun kill-everyone (&optional (population *the-population*))
  (mapc #'death (get population 'members))
  t)


;;Takes the parent's 'input and makes a new person from it, initializing
;;the person into *the-population*
;;
;;Right now naming scheme is parent-child, which doesn't allow for multiple children.
;;Also this makes multiple generations somewhat unwieldy.
;;
;;Could make the name be an input into the function, but geneologies wouldn't be 
;;as regulated... which I guess isn't really a problem if we trust the user...
;; PROBABLY NOT RIGHT FOR CURRENT CODE YET
(defun birth (parent)
  (let ((new-person (gentemp "P")))
    (make-person new-person (get parent 'group) nil (get parent 'input))
    (setf (get new-person 'parent) parent)
    (create-net new-person) ;;to initialise the new person
    new-person))
