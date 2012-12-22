; make sure nothing is ever considered settled:
(SETF *MIN-POP-TICKS-TO-SETTLE* (* 2 *MAX-POP-TICKS*))        ; for git branch seproadback
(SETF *MIN-SETTLE* (* *MAX-TIMES* *MIN-POP-TICKS-TO-SETTLE*)) ; for git branch master in 11/2012 up to at least early 12/2012

