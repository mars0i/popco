; Generalized logistic function for mapping map activation values
; to proposition link weights in POPCO.

; The definition is a for a full generalized logicistic function
; which is more complicated than necessary, but allows 
; flexibility for experimentation if needed.  
; I'm assuming that a good compiler will reduce it to something more 
; efficient, especially for the defconstants.  Might be worth testing
; this, though.  [Might be worth simplifying later anyway.]

; Note: The "sometimes called" parameter names come from the Wikipedia
; page on "generalized logistic function".

;; CURRENTLY MOVED TO variables.lisp:
; Maybe change them to constants later:

; steepness of curve in the slope-ey part:
;(defvar *logistic-growth-rate* 35)  ; sometimes called B

; Gives the middle value when tautness=1, etc.
;(defvar *logistic-position* .5) ; sometimes called M


; Higher values as it were pull on the left end of the rope;
; lower values > 0 as it were let go of tension on it.
; [This is my conception of this parameter, anyway.]
;(defconstant +logistic-taughtness+ 1) ; sometimes called v

; These can be inlined:

(defconstant +logistic-upper-asymptote+ 1) ; sometimes called K

(defconstant +logistic-lower-asymptote+ 0) ; sometimes called A

; Does same thing as *logistic-position*, but with exponential scale,
; and without interacting with growth rate:
(defconstant +logistic-exponential-position+ 1) ; sometimes called Q

; MAIN DEFINITION
(defun logistic (x)
  (+
    +logistic-lower-asymptote+
    (/
      (- +logistic-upper-asymptote+ +logistic-lower-asymptote+)
      (expt 
        (1+ (* +logistic-exponential-position+
               (exp (- (* +logistic-growth-rate+
                          (- x +logistic-position+))))))
        (/ 1 +logistic-taughtness+)))))

