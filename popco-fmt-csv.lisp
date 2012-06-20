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

(defun fmt-pop-propn-labels-csv-row (&optional (population *the-population*))
  (concatenate-tree (fmt-pop-propn-labels-csv population))) ; concatenate-tree in popco-fmt-utils.lisp

(defun fmt-pop-propn-labels-csv (&optional (population *the-population*))
  (mapcar #'fmt-person-propn-labels-csv 
        (symbol-sort (get population 'members))))

(defun fmt-person-propn-labels-csv (person)
  (mapcar #'fmt-unit-label-csv
        (symbol-sort (get person 'all-propositions))))

(defun fmt-unit-label-csv (unit)
  (format nil "~S," (symbol-name unit)))

;;;;;;;;;;;;;;;;;;;;
;; data row:

(defun fmt-pop-propn-activns-csv-row (&optional (population *the-population*))
  (concatenate-tree (fmt-pop-propn-activns-csv population))) ; concatenate-tree in popco-fmt-utils.lisp

(defun fmt-pop-propn-activns-csv (&optional (population *the-population*))
  (mapcar #'fmt-person-propn-activns-csv 
        (symbol-sort (get population 'members))))

(defun fmt-person-propn-activns-csv (person)
  (mapcar #'fmt-unit-activn-csv
        (symbol-sort (get person 'all-propositions))))

(defun fmt-unit-activn-csv (unit)
  (format nil "~S," (fmt-activation unit)))
