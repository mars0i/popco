

; NO THIS IS SILLY--The all-constraints property, when filled, is
; just a way of displaying the constraints.  The actual contraints
; are in one-way links in the properties of each node.
; This destructively modifies constraints in the POPCO version to
; have weights like those being produced in COHERE:
;(defun make-interesting ()
;  (mapc 
;    #'(lambda (cs) 
;        (let 
;          ((intr (car cs))
;           (eh (second cs))) 
;          (setf (cddr eh) (cddr intr)))) 
;    interesting-pairs))
