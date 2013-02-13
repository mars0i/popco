;; popco-macros.lisp
;; All POPCO-specific macros collected for loading prior to other files.
;; Some macros are elsewhere in specific files.

(defmacro weight-of-semantic-iff (semantic-iff) `(third ,semantic-iff))
(defmacro prop-of-conv (conversation) `(first ,conversation))
(defmacro speaker-of-conv (conversation) `(second ,conversation))
(defmacro listener-of-conv (conversation) `(third ,conversation))
(defmacro analog-struc-of-propn (propn) `(caar (get ,propn 'belongs-to))) ; there's only one such [like struc-from-propn in acme.lisp]
(defmacro analog-struc-of-pred (pred) `(car (get ,pred 'belongs-to)))
(defmacro analog-struc-of-obj (obj) `(car (get ,obj 'belongs-to)))     ; BUG CURRENTLY OBJS DON'T HAVE THIS
(defmacro speaker-of-cpair (conversepair) `(car ,conversepair))
(defmacro listener-of-cpair (conversepair) `(cadr ,conversepair))
(defmacro target-analog-strucs-of-person (person)
  `(remove-if-not #'target-analog-struc?
    (get ,person 'all-structures)))
(defmacro source-analog-strucs-of-person (person)
  `(remove-if #'target-analog-struc?
    (get ,person 'all-structures)))
(defmacro pop-members (pop) (get pop 'members))

; Overwrite definition in imp.lisp
; Better to set this in the model input file, if it's desired:
;(defmacro normalize-degree (degree) `(logistic ,degree))

(defmacro get-guess-socket (person) `(get ,person 'guess-socket))
(defmacro set-guess-socket (person sock) `(setf (get ,person 'guess-socket) ,sock))
; note can't use "socket" as param name in macro--conflicts with sbcl-sockets.lisp

; old versions:
;(defmacro person-of-pers-sock (person-socket) `(car ,person-socket))
;(defmacro socket-of-pers-sock (person-socket) `(cdr ,person-socket))

;(defmacro node1-of-constraint (constraint) `(car constraint))
;(defmacro node2-of-constraint (constraint) `(cadr constraint))
;(defmacro weight-of-constraint (constraint) `(car constraint))
