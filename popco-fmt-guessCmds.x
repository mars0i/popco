

(defun update-guess-edges-for-person (person-socket)
  (let ((person (person-of-pers-sock person-socket)))
    (mapc #'(lambda (constraint) (update-guess-edge constraint person-socket))
          (list-constraints (get person 'all-propositions)))))

(defun update-guess-edge (constraint person-socket)
  (let ((person (person-of-pers-sock person-socket))
        (socket (socket-of-pers-sock person-socket)))
    (if socket
      (tcp-print-raw socket (fmt-guess-edge-update-cmds constraint person))
      (format t "~%update-guess-edge: Connection to GUESS has not been established.~%"))))

(defun update-guess-nodes-for-persons (person-sockets)
  (mapc #'update-guess-nodes-for-person person-sockets))

(defun update-guess-nodes-for-person (person-socket)
 (mapc #'(lambda (unit) (update-guess-node unit person-socket))
  (get (person-of-pers-sock person-socket) 'all-units)))

;; Update a single GUESS node from the properties of a POPCO unit
(defun update-guess-node (unit person-socket)
  (let ((person (person-of-pers-sock person-socket))
        (socket (socket-of-pers-sock person-socket)))
    (if socket 
     (tcp-print-raw socket (fmt-guess-node-update-cmds unit person))
     (format t "~%update-guess-node: Connection to GUESS has not been established.~%"))))

(defun fmt-guess-edge-elt (constraint &optional (person *the-person*))
  (let ((node1 (personal-sym-to-guess-nodename (car constraint) person))
        (node2 (personal-sym-to-guess-nodename (cadr constraint) person)))
    (format nil "~A-~A," node1 node2)))

(defun fmt-guess-edge-final-elt (constraint &optional (person *the-person*))
  (let ((node1 (personal-sym-to-guess-nodename (car constraint) person))
        (node2 (personal-sym-to-guess-nodename (cadr constraint) person)))
    (format nil "~A-~A" node1 node2)))

