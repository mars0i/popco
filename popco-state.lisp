;; popco-state.lisp
;; Copyright (c) 2012 by Marshall Abrams
;; May be distributed only with permission from the author.
;; Functions for storing and restoring the entire state of a population.

;; Note that we're using backquote in defuns rather than defmacros in some cases below.
;; The point is to construct a code list without evaluating it.


; generate code that could restore a symbol's value and plist (doesn't handle symbol-functions)
(defun restore-sym-code (sym)
  (let ((code `(setf (symbol-plist ',sym) ',(symbol-plist sym))))
    (when (boundp sym)
      (setf code (list `(setf (symbol-value ',sym) ',(symbol-value sym))
                       code)))
    (cons 'progn code)))
