;;;; natsel.lisp
;;;; Authors: Kristen Hammack and Marshall Abrams
;;;;
;;;; Originally based on:
;;;; nat-selection1.lisp
;;;; sandbox for natural selection functionality in POPCO
;;;; Author:    Kristen Hammack
;;;; Vers:      1.0.0 08/2012 kmh - initial coding
;;;;            2.0.0 01/2013 kmh - deleted the model; just functions now.

;;;; TODO:
;;;; Possibly add simple ways to default social network structure from parents into offspring.
;;;; Add some basic fitness functons (possibly in a separate files).
;;;; Add code to main loop to cause birth and death as a function of fitness, and/or randomly.


(defun remove-from-group (person group)
  (setf (get group 'members) (remove person (get group 'members))))

;;;**********************
;;;DEATH AND BIRTH FUNCTIONS

;;Removes person from *the-population* and groups and uninterns the symbol for the person
(defun kill (person)
  (mapc #'(lambda (group) (remove-from-group person group)) *all-soc-net-groups*)
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
;; CURRENT VERSION DOES NOT COPY SOCIAL NETWORK STRUCTURE.  SHOULD IT?
;; AND SHOULD CHILDREN BE LINKED TO THEIR PARENT?  WHAT ABOUT THEIR SIBLINGS?
;; MAYBE THESE SHOULD BE OPTIONS OR SHOULD BE DONE IN WRAPPER FUNCTIONS THAT CALL THIS ONE.
;; MAYBE NOT RIGHT IN OTHER WAYS FOR CURRENT CODE YET, TOO.
(defun birth (parent)
  (let ((new-person (gentemp "P")))
    (make-person new-person (get parent 'group) nil (get parent 'input))
    (setf (get new-person 'parent) parent)
    (create-net new-person) ;;to initialise the new person
    new-person))

;; Note that birth makes the offspring of the same parent indeed functionally identical, as you'd expect.
;; Here p1 and p2 are offspring of bf01 in crime3socnet4netlogo.lisp:
;; (tree-diff 
;;   (maptree #'(lambda (s) (maybe-depersonalize-sym s 'p1)) (symbol-plist 'p1))
;;   (maptree #'(lambda (s) (maybe-depersonalize-sym s 'p2)) (symbol-plist 'p2)))
;; This creates a tree in which each element is T, indicating identity, except for some non-identical floats with identical printed representations.
;; If you make an offspring of a newborn parent, they will also be functionally the same except for the parent reference.  For non-newborns,
;; the story is different.
