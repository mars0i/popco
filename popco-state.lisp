;; popco-state.lisp
;; Copyright (c) 2012 by Marshall Abrams
;; May be distributed only with permission from the author.
;; Functions for storing and restoring the entire state of a population.



;; Note that we're using backquote in defuns rather than defmacros in some cases below.
;; The point is to construct a code list without evaluating it.


; Generate code that could restore a symbol's value and plist [can't handle symbol-functions]
; At present, few if any popco symbols have values--only plists--but can't hurt to add value-setting code.
; The general idea is that you produce a list that starts with setf, then a form referencing a data structure,
; into which we insert the current symbol name, and then a quoted form into which we insert the data that's
; currently in that data structure.
(defun restore-sym-code (sym)
  (let ((code `(setf (symbol-plist ',sym) ',(symbol-plist sym)))) ; we can always make code to set the plist, even if it's empty
    (when (boundp sym)                                            ; we can only generate code to set the symbol's value if it has a value now
      (setf code (list code                                       ; make a list containing the old plist form and a new set value form
                       `(setf (symbol-value ',sym) ',(symbol-value sym)))))
    (cons 'progn code))) ; add progn to run both forms
