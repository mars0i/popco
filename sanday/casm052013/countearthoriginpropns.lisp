

(defun list-persons-earth-origin-propns ()
  (mapcar #'(lambda (pers) 
              (cons pers 
                    (remove-if-not 
                      #'(lambda (propn-str) 
                          (equal "OE-" (subseq propn-str 0 3))) 
                      (mapcar #'(lambda (propn) 
                                  (symbol-name (personal-to-generic-sym propn pers)))
                              (get pers 'all-propositions)))))
          (get 'folks 'members)))
