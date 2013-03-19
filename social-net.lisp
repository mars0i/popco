;;;; social-net-functions.lisp
;;;; social networks in POPCO
;;;; Author:    Kristen Hammack
;;;; Vers:      1.0.0 12/2012 kmh - initial coding
;;;;            1.0.1 2/2012 MA moved make-person and choose-conversers to persons.lisp.
;;;; Other small mods by MA ...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;   SOCIAL NETWORKING REDEFINITIONS   ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; General notes -MA:
;;; 
;;; I believe that PUT-IN-GROUP is the lowest-level group/edge-making function,
;;; and that it's called, eventually, by all methods of creating groups, including
;;; the groups that are created automatically to represent directed edges between
;;; individual persons.  So if you want to affect all ways of creating groups, you 
;;; you can do it by modifying put-in-group.


;;-----------------------------------------------------
;; UTILITIES for debugging, investigation
(defun get-groups (p) (get p 'groups))

(defun get-talks-to (p) (get p 'talks-to))

;; List persons that person p can talk to:
(defun get-talks-to-persons (p)
  (remove-duplicates 
    (flatten 
      (mapcar #'get-members (get-talks-to p)))))

;; convenient fns to print results of preceding
(defun print-groups (p) (print (list p (get-groups p))))

(defun print-talks-to-persons (p)
  (print (list p (get-talks-to p))))

(defun print-talks-to-persons (p)
  (print (list p (get-talks-to-persons p))))

(defun print-talks-to-sorted (p)
  (print (list p 
               (sort 
                 (get-talks-to p)
                 #'symbol-lessp))))

(defun print-talks-to-persons-sorted (p) 
  (print (list p 
               (sort 
                 (get-talks-to-persons p)
                 #'symbol-lessp))))

;;-----------------------------------------------------
;; CHOOSE-CONVERSERS
;; Produce a list of persons who are to converse (probably only one-way eg 
;; first talks to second) i.e. a list of conversers, each containing two 
;; persons from the pop where noone appears in more than one pair. 
;; Modified by KH to reflect group/network structure.
;; ARGS:    population
;; RETURNS: a cons with a list of pairs (lists) of persons as car, 
;;          and the entire population as cdr
;; Formerly in popco.lisp.
;; For each in (population 'members), make (speaker 'num-listeners) randomized pairs from
;; the list (speaker 'talks-to) 

(defun choose-conversers (population) 
  (if (not *do-converse*)
      (cons nil population)
      (cons (apply #'append
                   (mapcar #'make-converser-pairs (get population 'members)))
            population)))


;; Guts of choose-conversers
(defun make-converser-pairs (speaker)
  "Returns a list of converser-pairs (lists with SPEAKER as the car and a listener as the cdr).
The number of pairs returned will be the minimum of the speaker's 'NUM-LISTENERS and the length
of his GET-CONVERSERS list."
  (let* ((randomized-conversers (copy-shuffle (get-conversers speaker)))
         (num-people (length randomized-conversers))
         (num-listening (get speaker 'num-listeners))
         (num-conversers (min num-listening num-people))
         (actual-conversers (last randomized-conversers num-conversers)) ; CHANGE LAST TO e.g. SUBSEQ
         (speaker-list (make-list num-conversers :initial-element speaker)))
    (mapcar #'list speaker-list actual-conversers)))


(defun get-conversers (person)
  "Returns a list of all the people PERSON talks to."
  (remove person
          (remove-duplicates
           (apply #'append 
                  (mapcar #'get-members (get person 'talks-to))))))


(defun get-members (group)
  "Returns a list of the members in a group."
  (get group 'members))


(defun put-in-group (person group)
  "Puts PERSON in GROUP if s/he is not already there.
Also adds GROUP to PERSON's 'GROUPS property.
RETURNS: Person's groups after everything is done.
SPECIAL BEHAVIOR:  If GROUP is NIL, puts PERSON in *THE-POPULATION*
but does not add *THE-POPULATION* to PERSON's 'GROUPS property."
  (if group
      (progn
        (setf *all-soc-net-groups* (cons-if-new group *all-soc-net-groups*))
        (setf (get group 'members) (cons-if-new person (get group 'members)))
        (setf (get person 'groups) (cons-if-new group (flatten (list (get person 'groups)))))) ; FLATTEN LIST to deal with if value of groups is a symbol and not a list
      (setf (get *the-population* 'members) (cons-if-new person (get *the-population* 'members))))
  (get person 'groups))


(defun put-in-groups (person list-of-groups)
  "Puts PERSON in each of LIST-OF-GROUPS.
If LIST-OF-GROUPS is nil, puts PERSON in *THE-POPULATION."
  (if (atom list-of-groups) ; Don't know which to expect more often so went with same style as above
      (put-in-group person list-of-groups)
      (mapc #'put-in-group 
              (make-list
               (length list-of-groups)
               :initial-element person)
              list-of-groups)))


(defun make-merged-groups (list-of-lists &optional (population *the-population*))
  "Takes a list of lists of persons, creates a new group for each list, and merges
the groups together into POPULATION.
SEE: MERGE-GROUPS, MAKE-GROUPS"
  (merge-groups
   (flatten (list (make-groups list-of-lists)))
   population))


(defun merge-groups (list-of-groups &optional (population *the-population*))
  "Takes a list of groups and merges them, sets POPULATION (default *THE-POPULATION*)
to have the new list of persons as its 'MEMBERS"
  (setf (get population 'members)
        (remove-duplicates
         (append (get population 'members)
                 (flatten
                  (apply #'append (mapcar #'get-members list-of-groups)))))))


(defun make-groups (list-of-lists)
  "Takes a list of lists of existing persons and creates a new group for each list of persons,
with all persons having their 'TALKS-TO eq their 'GROUPS."
  (mapcar #'make-talking-group list-of-lists))

(defun make-talking-group (list-of-persons)
  "Helper function to MAKE-GROUPS"
  (mapcar #'put-in-group-and-talks-to 
        list-of-persons 
        (make-list
         (length list-of-persons)
         :initial-element (gentemp "G")))) ; btw gentemp is deprecated

;; (Why is gentemp deprecated?  Because it can cause bugs if your code thinks it is
;; creating a new symbol which, as it turns out, is the same as one created by gentemp.
;; e.g. if you write a gentemp'ed symbol to a file and then create symbols based on
;; what's in that file later, and then call gentemp separately .... You see the problem.
;; The other part of the rationale for removing gentemp from the language is that
;; it's easy to write a replacement function by hand.  Which is probably what I'll
;; do if they ever take it out of the language.
;; -MA 2/2013)

(defun put-in-group-and-talks-to (person group)
  "Puts PERSON in GROUP 'MEMBERS
Puts GROUP in PERSON 'GROUPS
Puts GROUP in PERSON 'TALKS-TO"
  (put-in-group person group)
  (setf (get person 'talks-to) (cons-if-new group (get person 'talks-to))))


;;; Can't decide whether what this returns is useful or not... Seems like it might be.
(defun make-ungrouped-directed-pairs (list-of-pairs)
  "Takes a list of pairs of existing persons and makes directed links between those persons
as specified by the pair (SPEAKER LISTENER).
A new, arbitrarily named group is created for each LISTENER.
RETURNS: A list of all of the groups in the 'TALKS-TO property of every SPEAKER specified."
  (remove-duplicates (flatten (list (mapcar #'make-directed-pair list-of-pairs)))))


(defun make-directed-pair (speaker-listener &optional (group (gentemp "G")))
  "Puts LISTENER in GROUP (default: arbitarily-named new group) and adds GROUP to SPEAKER'S 'TALKS-TO"
  (let ((speaker (first speaker-listener))
        (listener (second speaker-listener)))
    (setf (get speaker 'talks-to)
          (remove-duplicates
            (append (put-in-group listener group)
                    (get speaker 'talks-to))))))


;;; I know this is bad style and I don't know why it returns nil, but it seems to be doing
;;; what I want it to do.
(defun make-grouped-directed-pairs (list-of-pairs)
  "Takes a list of pairs of existing persons and makes directed links between those persons
as specified by the pair (SPEAKER LISTENER).
A new, randomly named group is created for each SPEAKER, into which each LISTENER of that
SPEAKER is placed. "
  (let ((oldperson)
        (thegroup)
        (result))
    (dolist (speaker-listener (sort (copy-seq list-of-pairs) #'string< :key #'car))
      (unless (eq oldperson (first speaker-listener))
        (setf oldperson (first speaker-listener))
        (setf thegroup (gentemp "G")))
      (append result (make-directed-pair speaker-listener thegroup)))
    result))

;(format t "Networking Functions Loaded")
