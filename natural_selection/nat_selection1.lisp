;nat_selection1.lisp
;sandbox for natural selection functionality in POPCO
;Author:    Kristen Hammack
;Vers:      1.0.0 08/2012 kmh - initial coding
;	    2.0.0 01/2013 kmh - deleted the model; just functions now.




;;;**********************
;;;DEATH AND BIRTH FUNCTIONS

;;Removes person from *the-population* and uninterns the symbol for the person
;;I think this still introduces a memory leak, but maybe sbcl is smart enough to kill the
;;symbol after a load?
(defun death (person &optional (population *the-population*))
  (cond ((eq (get person 'group) population) 
         (setf (get population 'members) (remove person (get population 'members)))
         (unintern person)
         (get population 'members))
        (t (error "~%~a is not a member of ~a~%" person population))))

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
(defun birth (parent)
  (let ((new-person (gentemp "P")))
    (make-person new-person (get parent 'group) nil (get parent 'input))
    (setf (get new-person 'parent) parent)
    (create-net new-person) ;;to initialise the new person
    new-person))

(format t "Death and Birth Functions Loaded")