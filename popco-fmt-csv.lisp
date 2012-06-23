;; popco-fmt-csv.lisp
;; Copyright (c) 2012 by Marshall Abrams
;; May be distributed only with permission from the author.
;; Data-formatting functions for output to csv files

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; formatting proposition name/activation data at a given time for output
;; to a csv file.  Desired format is a single row of activations sorted
;; by person name and by proposition name within person.  If proposition
;; names have consistent prefixes, this will also group propositions into
;; source vs. target, domains within source/target, etc.
;; Then each row will correspond to a different pop-tick, i.e. time.

;;;;;;;;;;;;;;;;;;;;
;; header/label row:

;; RECORD-POSS-PERSONAL-PROPNS-IN-POP 
;; NOTE: This should be called first before fmt-pop-propn-labels-csv-row or fmt-pop-propn-activns-csv-row.
;; We should record the possible personal propositions so that we can use them for each row
(defun record-poss-personal-propns-in-pop (&optional (population *the-population*))
  (setf (get population 'possible-personal-propositions)
        (poss-personal-propns-in-pop population)))

;; FMT-POP-PROPN-LABELS-CSV-ROW 
;; Make a header row of column labels naming personal propositions for proposition activation data
(defun fmt-pop-propn-labels-csv-row (&optional (population *the-population*))
  (let ((poss-propns (get population 'possible-personal-propositions)))
    (when (null poss-propns)  ; guard against calling before possible propositions has been recorded
      (error "fmt-pop-propn-labels-csv-row: No propositions in population ~S's possible-personal-propositions property" population))
    (concatenate-tree                                     ; concatenate-tree in popco-fmt-utils.lisp
      (mapcar-with-tail #'fmt-unit-label-csv              ; mapcar-with-tail in popco-utils.lisp
                        #'fmt-last-unit-label-csv
                        poss-propns))))

;; FMT-UNIT-LABEL-CSV 
;; csv string for a unit/node name other than the last one
(defun fmt-unit-label-csv (unit)
  (format nil "~S," (symbol-name unit)))

;; FMT-LAST-UNIT-LABEL-CSV 
;; csv string for a unit/node name that's the last one
(defun fmt-last-unit-label-csv (unit)
  (format nil "~S" (symbol-name unit)))

;; Note: All the appending, sorting, and duplicate-removing below is the kind of thing 
;; that's relatively expensive, but we only have to do it once at the beginning of
;; a simulation run, so it's not worth trying to make it more efficient.

;; POSS-PERSONAL-PROPNS-IN-POP 
;; Return all possible personal propositions, sorted alphabetically (i.e. by person and 
;; proposition name within person).  "Possible" means that it's the proposition name
;; created by adding a person name to a generic-propn name, for all persons in the pop,
;; and generic versions of all propns in the pop.
(defun poss-personal-propns-in-pop (&optional (population *the-population*))
  (sort-copy ; in popco-utils-lisp
    (cross-mapcar #'generic-to-personal-sym          ; cross-mapcar in popco-utils.lisp
                  (generic-propns-in-pop population)
                  (get population 'members))
    #'symbol-lessp))                                 ; also in popco-utils.lisp

;; GENERIC-PROPNS-IN-POP 
;; return an unsorted list of generic proposition versions of all propositions in population
(defun generic-propns-in-pop (&optional (population *the-population*))
  (remove-duplicates
    (genericized-propns-in-pop (get population 'members))))

;; GENERICIZED-PROPNS-IN-POP-AUX 
;; Returns all propositions in members of population, converted to generic propns.
;; Does not remove duplicates.
(defun genericized-propns-in-pop (members)
  (if (null members)
    ()
    (let ((person (car members)))
      (append
        (mapcar #'(lambda (propn) (personal-to-generic-sym propn person))
                (get person 'all-propositions))
        (genericized-propns-in-pop (cdr members))))))


;;;;;;;;;;;;;;;;;;;;
;; data row:

;; FMT-POP-PROPN-ACTIVNS-CSV-ROW 
;; Generate a string in csv format which records the activations of all possible personal propositions 
;; in the order in which they're listed in poss-propns.  Personal propositions which don't exist yet
;; will be represented as having activation 0.0.
(defun fmt-pop-propn-activns-csv-row (&optional (population *the-population*))
  (let ((poss-propns (get population 'possible-personal-propositions)))
    (when (null poss-propns)  ; guard against calling before possible propositions has been recorded
      (error "fmt-pop-propn-activns-csv-row: No propositions in population ~S's possible-personal-propositions property" population))
    (concatenate-tree                                     ; concatenate-tree in popco-fmt-utils.lisp
      (mapcar-with-tail #'fmt-unit-activn-csv              ; mapcar-with-tail in popco-utils.lisp
                        #'fmt-last-unit-activn-csv
                        poss-propns))))
; note that since POPCO currently has all units in the top-level namespace, we don't need to go inside persons to get the activations

(defun fmt-unit-activn-csv (unit)
  (format nil "~S," (fmt-activation unit))) ; fmt-activation will return 0.0 if no such unit has been constructed

(defun fmt-last-unit-activn-csv (unit)
  (format nil "~S" (fmt-activation unit)))  ; fmt-activation will return 0.0 if no such unit has been constructed
