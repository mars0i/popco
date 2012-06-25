
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
