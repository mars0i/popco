;social-net-functions.lisp
;sandbox for social networks in POPCO--redefining make-person and choose-conversers
;Author:    Kristen Hammack
;Vers:      1.0.0 12/2012 kmh - initial coding


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;   SOCIAL NETWORKING REDEFINITIONS   ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;Originally defined in consensus.lisp
;;;Changed to add 'talks-to and 'num-listeners for networking 12/2012
;;;Changed to allow for multiple groups 1/2013
(defun make-person (person list-of-groups given initial-input &optional addl-input converse-strucs (talks-to list-of-groups) (num-listeners 1))
  "Makes a person with the given parameters.
REQUIRED: *the-population* MUST BE DEFINED.

(Maybe) Surprising Behavior: If LIST-OF-GROUPS is NIL and TALKS-TO is not given,
(get PERSON 'GROUPS) and (get PERSON 'TALKS-TO)will return NIL, but (get *THE-POPULATION* 'members)
will return a list with PERSON in it, i.e. PERSON will be in a group but will not 'know' it, and
s/he will not initiate conversation."
  (initialize-person-properties person)  ; From popco.lisp. Note: setfs *the-person* to person
  (put person 'groups list-of-groups)
  (put-in-groups person list-of-groups) ;put person in each of the groups
  ;(put group 'members (cons-if-new person (get group 'members))) ; changed push to cons-if-new  -MA 6/2011
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
                  (get old 'groups)
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

;;Guts of choose-conversers
(defun make-converser-pairs (speaker)
  "Returns a list of converser-pairs(lists with SPEAKER as the car and a listener as the cdr).
The number of pairs returned will be the minimum of the speaker's 'NUM-LISTENERS and the length
of his GET-CONVERSERS list."
  (let* ((randomized-conversers (randomize (get-conversers speaker)))
         (num-people (length randomized-conversers))
         (num-listening (get speaker 'num-listeners))
         (num-conversers (min num-listening num-people))
         (actual-conversers (last randomized-conversers num-conversers)) ; CHANGE LAST TO e.g. SUBSEQ
         (speaker-list (make-list num-conversers :initial-element speaker)))
    (mapcar #'list speaker-list actual-conversers)))



(defun get-conversers (person)
  "Returns a list of all the people PERSON talks to."
  (remove person
    (apply #'append 
      (mapcar #'get-members(get person 'talks-to)))))


(defun get-members (group)
  "Returns a list of the members in a group."
  (get group 'members))


(defun put-in-group (person group)
  "Puts PERSON in GROUP if s/he is not already there."
  (put group 'members (cons-if-new person (get group 'members))))


(defun put-in-groups (person list-of-groups)
  "Puts PERSON in each of LIST-OF-GROUPS.
If LIST-OF-GROUPS is nil, puts PERSON in *THE-POPULATION."
  (if list-of-groups
      (if (atom list-of-groups) ;Don't know which to expect more often so went with same style as above
          (put-in-group person list-of-groups)
          (mapcar #'put-in-group 
                  (make-list
                   (length list-of-groups)
                   :initial-element person)
                  list-of-groups))
      (put-in-group person *the-population*)))


(defun merge-groups (list-of-groups &optional (population *the-population*))
  "Takes a list of groups and merges them, sets the given population (default *the-population*)
to have the new list of persons as its 'members"
  (setf (get population 'members)
        (append (get population 'members)        
                (remove-duplicates
                 (flatten
                  (apply #'append (mapcar #'get-members list-of-groups)))))))


(format t "Networking Functions Loaded")