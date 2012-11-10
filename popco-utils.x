
  (cond ((eq unit (first semantic-iff)) 0)
        ((eq unit (second semantic-iff)) 1)
        t nil))

(defun random-subset (size superset)
  (let ((to-choose-from superset)
        (chosen '()))
    (dotimes (ignored size)
      (when (plusp (length to-choose-from)) ; a trivial inefficiency--keep going but don't do anything if we've used them all up
        (let ((to-move (elt to-choose-from (random (length to-choose-from)))))
          (setf chosen (cons to-move chosen))
          (setf to-choose-from (remove to-move to-choose-from)))))))

(defun find-semantic-iffs-in-unit-pairs (semantic-iffs unit-pairs)
  (mapcar #'(lambda (unit-pair)
              (find-semantic-iffs-by-units semantic-iffs 
                                           (first unit-pair) 
                                           (second unit-pair)))
          unit-pairs))
;; ALSO NOT RIGHT
(defun find-semantic-iffs-in-unit-pairs-aux (semantic-iffs unit-pairs)
  (if (null unit-pairs)
    nil
    (let* ((unit-pair (car unit-pairs))
           (sem-iff (find-semantic-iffs-by-units semantic-iffs (first unit-pair) (second unit-pair))))
      (if sem-iff
        (cons sem-iff (find-semantic-iffs-in-unit-pairs-aux semantic-iffs (cdr unit-pairs)))
        (find-semantic-iffs-in-unit-pairs semantic-iffs-aux (cdr unit-pairs))))))
