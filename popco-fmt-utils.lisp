;; popco-fmt-utils.lisp
;; Copyright (c) 2012 by Marshall Abrams
;; May be distributed only with permission from the author.
;; Data-formatting functions

(defvar *cause-string-for-others* "_cs_")
(defvar *prevent-string-for-others* "_pv_")
(defvar *map-string-for-others* "_mp_") ; string that will replace the "=" in ACME map unit names 
(defvar *minus-string-for-others* "_") ; string that will replace "-" in ACME unit names

; cl-ppcre:regex-replace's syntax makes multiple embedded calls of it hard to read;
; this reformulation fixes that, with syntax more like CL's SUBSTITUTE:
(defmacro regex-substitute (regex to-replace to-replace-in)
  `(cl-ppcre:regex-replace ,regex ,to-replace-in ,to-replace))
(defmacro regex-substitute-all (regex to-replace to-replace-in)
  `(cl-ppcre:regex-replace-all ,regex ,to-replace-in ,to-replace))

(defun fmt-activation (unit)
  (if (activation unit)
    (coerce (activation unit) 'short-float)
    0.0))
; note that non-existent units still exist as symbols if they are ever referenced,
; and if passed to this function, they are obviously referenced, but the macro
; activation just does (get unit 'activation), which will return nil if there is
; no such property.

; note that this can fail in some Lisps such as LispWorks and CLISP which have a limit
; on the max number of arguments that a function can take.
(defun concatenate-tree (tree)
  (let ((strings (flatten tree))) ; flatten is in popco-utils.lisp
    (if (atom strings)
      strings
      (apply #'concatenate (cons 'string strings))))) ; can break in Lisps with max arg list size

; CL doesn't have a string-replace, so we'll use regexps, even though it's more than needed.
; Format a popco symbol name string (e.g. a propn name) so that it's more
; likely to be acceptable to non-lisp programs such as R and GUESS:
; ARGUMENT SYM-NAME SHOULD BE A STRING AS PRODUCED BY SYMBOL-NAME OR (FORMAT "~A" SYM).
(defun normalize-sym-name-for-others (sym-name)
  (regex-substitute-all "-" *minus-string-for-others*
    (regex-substitute "=" *map-string-for-others*
      (regex-substitute-all "->" *cause-string-for-others*    ; -all needed for map nodes
        (regex-substitute-all "->-" *prevent-string-for-others* sym-name)))))
