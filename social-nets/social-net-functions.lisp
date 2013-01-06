;social-net-functions.lisp
;sandbox for social networks in POPCO--redefining make-person and choose-conversers
;Author:    Kristen Hammack
;Vers:      1.0.0 12/2012 kmh - initial coding


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;   SOCIAL NETWORKING REDEFINITIONS   ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;Originally defined in consensus.lisp--> Changed to add 'talks-to and 'num-listeners for networking
(defun make-person (person group given initial-input &optional addl-input converse-strucs (talks-to (list group)) (num-listeners 1))
  (initialize-person-properties person)  ; From popco.lisp. Note: setfs *the-person* to person
  (put person 'group group)
  (put group 'members (cons-if-new person (get group 'members))) ; changed push to cons-if-new  -MA 6/2011
  (put person 'given-el given) ; NOTE PROPNS ARE IN FULL SUBJ/PRED MESSAGE FORMAT
  ; (put person 'other-el other)
  (put person 'input initial-input)
  (when addl-input 
    (put person 'addl-input addl-input))
  (put person 'converse-strucs (mapcar #'generic-to-personal-sym converse-strucs))
  (put person 'talks-to talks-to) ;New for networking--groups that person talks to
  (put person 'num-listeners num-listeners) ;New for networking--number of listeners a person has (a radio dj only reaches a percentage of the people who might listen to him)
  person)

;;;Originally defined in consensus.lisp--> Changed to add 'talks-to and num-listeners for networking
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
                          (get old 'converse-strucs))
                  (get old 'talks-to)
                  (get old 'num-listeners))
     result)))

;;;Originally defined in popco.lisp
;;For each in (population 'members), make (speaker 'num-listeners) randomized pairs from
;;the list (speaker 'talks-to) 
(defun choose-conversers (population) 
  (if (not *do-converse*)
      (cons nil population)
      (cons (apply #'append
                   (mapcar #'make-converser-pairs (get population 'members)))
            population)))


;;;;;;;;;;;;;;;;;;;;;;;   NEW FUNCTIONS   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Guts of choose-conversers--probably want to make this more lisp-y too. -KMH
(defun make-converser-pairs (speaker)
  (let* ((randomized-conversers (randomize (get-conversers speaker)))
         (num-people (length randomized-conversers))
         (num-listening (get speaker 'num-listeners))
         (num-conversers (min num-listening num-people))
         (actual-conversers (last randomized-conversers num-conversers)) ; CHANGE LAST TO e.g. SUBSEQ
         (speaker-list (make-list num-conversers :initial-element speaker)))
    (mapcar #'list speaker-list actual-conversers)))



;;Loops over all the groups in 'talks-to and does (get group 'members)
(defun get-conversers (person)
  "Returns a list of all the people PERSON talks to."
  (remove person
    (apply #'append 
      (mapcar #'get-members(get person 'talks-to)))))

(defun get-members (group)
  (get group 'members))


;;Takes a list of groups and merges them, sets the given population (default *the-population*)
;;to have the new list of persons as its 'members
(defun merge-groups (list-of-groups &optional (population *the-population*))
  (setf (get population 'members) 
        (apply #'append (mapcar #'get-members list-of-groups))))


(format t "Networking Functions Loaded")