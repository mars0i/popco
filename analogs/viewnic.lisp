(defun pppropns ()
  (pplist 'polly_h1=c1) (pplist 'polly_i1=c1)
  (pplist 'polly_h2=c2) (pplist 'polly_i2=c2)
  (pplist 'polly_h3=c3) (pplist 'polly_i3=c3)
  (pplist 'polly_h4=c4) (pplist 'polly_i4=c4)
  (pplist 'polly_h5=c5) (pplist 'polly_i5=c5)
  (pplist 'polly_h6=c6) (pplist 'polly_i6=c6)
)

(defun print-sorted-units (units) 
  (print-units
        (stable-sort (copy-list units) 
                     #'(lambda (x y) (> (activation x) (activation y)))))
  t)

(defun print-units (units)
  (mapc (lambda (s) (format t "~S ~S~%" s (activation s))) units))

(defun yo () 
  (print-units
    '(polly_h1=c1
      polly_i1=c1
      polly_h2=c2
      polly_i2=c2
      polly_h3=c3
      polly_i3=c3
      polly_h4=c4
      polly_i4=c4
      polly_h5=c5
      polly_i5=c5
      polly_h6=c6
      polly_i6=c6
      polly_support-yes=support?
      polly_support-no=support?
      polly_terrorists-yes=Contra-terror?
      polly_terrorists-no=Contra-terror?
      polly_freedom-fighters-yes=Contra-freedom?
      polly_freedom-fighters-no=Contra-freedom?)))
