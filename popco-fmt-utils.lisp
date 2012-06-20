;; popco-fmt-csv.lisp
;; Copyright (c) 2012 by Marshall Abrams
;; May be distributed only with permission from the author.
;; Data-formatting functions

(defun fmt-activation (unit)
  (if (activation unit)
    (coerce (activation unit) 'short-float)
    0.0))

; note that this can fail in some Lisps such as LispWorks and CLISP which have a limit
; on the max number of arguments that a function can take.
(defun concatenate-tree (tree)
  (let ((strings (flatten tree))) ; flatten is in popco-utils.lisp
    (if (atom strings)
      strings
      (apply #'concatenate (cons 'string strings))))) ; can break in Lisps with max arg list size
