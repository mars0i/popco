;;; HTsOriginalSocratesForPOPCOutilities.lisp
;;; utilities for reporting results

;(format t "Defining list of all possible map units from prag's all-units ... ~%")
(setf *the-person* 'prag)
(defvar soc-maps (mapcar #'personal-to-generic-sym (get 'prag 'all-units)))

(load "analogs/HTsOriginalSocratesAllMapUnits")

(defun print-soc-activns-alpha ()
  (format t "unit iso     non-iso w/prag~%")
  (mapc 
    (lambda (u) (format t "~S	~,2f	~,2f	~,2f~%" 
                        u 
                        (progn (setf *the-person* 'iso)(activation (generic-to-personal-sym u)))
                        (progn (setf *the-person* 'non)(activation (generic-to-personal-sym u)))
                        (progn (setf *the-person* 'prag)(activation (generic-to-personal-sym u)))))
    (stable-sort (copy-list soc-maps) #'symbol-lessp))
  t)

; for sorting by activation of the iso unit corresponding to a generic unit
(defun soc-units-in-iso-greaterp (u1 u2)
  (setf *the-person* 'iso)
  (soc-units-greaterp u1 u2))

;; ASSUMES *THE-PERSON* IS SET CORRECTLY
(defun soc-units-greaterp (u1 u2)
  (let ((pu1 (generic-to-personal-sym u1))
        (pu2 (generic-to-personal-sym u2)))
    (cond ((and (activation pu1) (activation pu2))  ; if both units have activations
           (activn-greaterp pu1 pu2))               ; use activn-greaterp
          ((activation pu1) t)                      ; if u1 has activn, u2 not, u1 is ">" u2
          ((activation pu2) nil))))                  ; if u1 doesn't but u2 does--or doesn't--u2 is not ">" u1

; print activations in all three persons in order of targets as listed 
; in Table 20 in H&T1989, p. 346,
; sorted within target by activation value in the isomorphic analogy:
(defun print-soc-activns-table20 (&optional (decimals 9))
  (format t "unit	iso	non-iso	w/prag~%")
  (mapc (lambda (target-list) 
          (mapc (lambda (u) 
                  (let ((iso-activn  (progn (setf *the-person* 'iso)  (activation (generic-to-personal-sym u))))
                        (non-activn  (progn (setf *the-person* 'non)  (activation (generic-to-personal-sym u))))
                        (prag-activn (progn (setf *the-person* 'prag) (activation (generic-to-personal-sym u)))))
                    (if iso-activn
                      (format t "~S	~,vf	~,vf	~,vf~%"
                              u decimals iso-activn decimals non-activn decimals prag-activn) ; when iso-activn exists, format it
                      (format t "~S	~S	~,vf	~,vf~%"
                              u          iso-activn decimals non-activn decimals prag-activn)))) ; otherwise use a directive that can hold the nil
                (stable-sort (copy-list target-list) #'soc-units-in-iso-greaterp))
          (terpri))
        soc-all-maps)
  t)

; print activations for one person in order of targets as listed in Table 20 in H&T1989, p. 346:
(defun print-soc-activns-table20-for-person (person &optional decimal-places)
  (setf *the-person* person)
  (format t "unit	activation~%")
  (mapc (lambda (target-list) 
          (mapc (lambda (u) 
                  (let ((unit (generic-to-personal-sym u)))
                    (if (activation unit)
                      (if decimal-places
                        (format t "~S	~,vf~%" unit decimal-places (activation unit)) ; when activn exists, format it; otherwise
                        (format t "~S	~f~%" unit (activation unit)))  ; if decimal places unspecified, use full representation
                      (format t "~S	~S~%"   unit (activation unit)))))  ; use a directive that can hold the nil, i.e. for the isomorphic scheme
                (stable-sort (copy-list target-list) #'soc-units-greaterp)) ; note that the predicate uses *the-person*
          (terpri))
        soc-all-maps)
  t)

