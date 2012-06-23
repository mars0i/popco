
(defun earth-to-sky-pop-no-neg (output-basename addl-ticks num-extra-persons num-to-flip add-negative-salience)
  (let ((anti-flip-fn (if add-negative-salience #'deparentize-person nil)))
    (collect-and-continue-run #'make-skyless-person sky-origin-propns output-basename addl-ticks num-extra-persons num-to-flip #'hunterize-person anti-flip-fn)))

