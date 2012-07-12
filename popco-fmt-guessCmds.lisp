;; popco-fmt-guessCmds.lisp
;; Copyright (c) 2012 by Marshall Abrams
;; May be distributed only with permission from the author.

;; Generates GUESS Python commands for updating an existing
;; network in GUESS, e.g. via a telnet connection.

;; THE TELNET CODE USES SBCL-SPECIFIC FUNCTIONS FROM PACKAGE SB-BSD-SOCKETS.
;; Must first load sbclsockets.lisp.

;; Formatting code uses functions from popco-fmt-guessML-specific?.lisp


(defvar *pop-tick-label* "tick") ; What each pop-tick is described as in Guess

; Notes on layouts:
;
; binPack() separates unconnected subgraphs which overlap on the screen,
; and pulls them together if they were far apart.  It does not seem to alter
; arrangements within unconnected subgraphs.
;
; center() makes the graph as large as possible while making sure that all 
; nodes are visible in the window.
;
; radialLayout(node), e.g. radialLayout(SALIENT) does not affect nodes which
; with no path to the chosen center node.  This means you can apply one algorithm
; to the whole graph in order to arrange the nodes in the analogy network and 
; proposition nodes which are in their own subgraphs, and then use radialLayout
; to lay out the nodes with paths to SALIENT.  It's probably best to use
; binPack as well.
;
; gemLayout() and physicsLayout(iterations) are similar, but physicsLayout is less
; likely to turn modify the graph unnecessarily.  However, you have to tune the
; number of iterations to get a good effect without taking too much time.
;
; Fruchterman-Rheingold layouts: frLayout() is fast; jfrLayout() is slow.  
; Neither takes any parameters.
;
; Kamada-Kawai layouts: Only jkkLayout1() seems to work for my purposes.  
; It's a nice layout for the analogy network, but it's too slow to be very
; useful in real time.  Useful for an initial layout of the analogy network
; when all persons start with all of the propositions--in which case the
; analogy network won't change.
; (Documentation says that kkLayout() fails for unconnected subgraphs, and
; maybe that's the problem with jkkLayout2() as well.)
;
; isomLayout() is not bad, either for the analogy network or for the whole
; graph.  With no parameters it's very slow, but 

;(defvar *guess-layout-commands* "physicsLayout(500)~%binPackLayout()~%center()~%")
;(defvar *guess-layout-commands* "gemLayout()~%binPackLayout()~%center()~%")
;(defvar *guess-layout-commands* "radialLayout(SALIENT)~%center()~%")
;(defvar *guess-layout-commands* "physicsLayout(500)~%radialLayout(SALIENT)~%binPackLayout()~%center()~%")
;(defvar *guess-layout-commands* "radialLayout(SALIENT)~%")
;(defvar *guess-layout-commands* "radialLayout(SALIENT)~%binPackLayout()~%center()~%")
(defvar *guess-layout-commands* "mdsLayout()~%radialLayout(SALIENT)~%binPackLayout()~%center()")

(defvar *guess-scripts-path* 
  (concatenate 'string 
               (sb-ext:posix-getenv "HOME")
               "/docs/phil/software/cohere/mine/guess"))

(defvar *first-port* 2222)
(defvar *extra-meta-interval* 10) ; Number of pop-ticks after which to apply extra commands; nil to ignore.
(defvar *extra-meta-commands* "center()~%") ; Commands to send after same interval:
(defvar *guess-file-extension* ".graphml")
(defvar *output-file-extension* ".out")

; convenience function:
(defun report-all ()
  (setf *persons-reporting-to-guess* (get *the-population* 'members)))

; This assumes that graphml files are named <person>.<guess-file-extension>,
; where <person> is the lowercase representation of the person's name.
; [The upper/lower distinction won't matter in case-insensitive OS's such as
; OS X and Windows, but could matter in traditional unixes such as Linux.]
(defun telguess (path person &optional (port-increment 0) (first-port *first-port*))
  (let* ((person-name (string-downcase (symbol-name person)))
         (guess-graphml-file 
           (concatenate 'string path person-name *guess-file-extension*))
         (guess-output-file 
           (concatenate 'string path person-name *output-file-extension*))
         (guess-program "popguess")
         (guess-telnet-script 
           (format nil "~A/telnet~S.py" *guess-scripts-path* (+ first-port port-increment))))
    (format t "~A ~A ~A~%" guess-program guess-graphml-file guess-telnet-script) ; DEBUG
    (sb-ext:run-program guess-program
                        (list guess-graphml-file guess-telnet-script)
                        :output guess-output-file
                        :search t
                        :wait nil
                        :if-output-exists :supersede)))

; recursive version of telguess--clear and can sleep between
(defun telguesses (path &optional (persons *persons-reporting-to-guess*))
  (when persons
    (telguesses-aux path persons 0)))

; odd cdr-test recursion allows sleeping only between telguess calls
(defun telguesses-aux (path persons port-increment)
  (telguess path (car persons) port-increment)
  (when (cdr persons)
    (sleep 1) ; otherwise, when Java is first loaded after boot, all but one load fails
    (telguesses-aux path (cdr persons) (+ 1 port-increment))))

;; map version - can't easily pause only in between
; side-effecting the index is clearer here than a do loop or recursion
;(defun telguesses (path &optional (persons *persons-reporting-to-guess*))
;  (let ((i -1))
;    (mapc #'(lambda (person) (telguess path person (incf i)) (sleep 1))  persons)))

;; loop version is hard to read
;(defun telguesses (path &optional (persons *persons-reporting-to-guess*))
;  (do ((i 0 (incf i))
;       (ps persons (cdr persons))
;       (p (car ps) (car ps)))
;      ((null ps) persons)
;    (telguess path p)
;    (when (cdr ps) (sleep 1)))) ; pause between persons--not before them all, not after them all

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Format a string containing GUESS commands to update a GUESS node based on properties of a POPCO unit
;; NOTE *THE-PERSON* must be set properly.
(defun fmt-guess-node-update-cmds (unit &optional (person *the-person*))
  (let* ((guess-nodename (personal-sym-to-guess-nodename unit person))
         (activn (activation unit))
         (color (guess-node-color unit))
         (size (guess-node-size unit)))
    (format nil 
            " ~A.activation = ~f ~% ~A.size = ~f ~% ~A.color = ~A ~%"
            guess-nodename activn
            guess-nodename size
            guess-nodename color)))

;; Format a string of commands to create a new GUESS node based on properties of a POPCO unit
;; NOTE *THE-PERSON* must be set properly.
(defun fmt-guess-node-add-cmds (unit &optional (person *the-person*))
  (format nil " addNode(\"~A\")~%" (personal-sym-to-guess-nodename unit person)))

;; Format a string of comments to remove a GUESS node
;; NOTE *THE-PERSON* must be set properly.
(defun fmt-guess-node-remove-cmds (unit &optional (person *the-person*))
  (format nil "removeNode(~A)~%" (personal-sym-to-guess-nodename unit person)))

;; Format a string containing GUESS commands to update a GUESS edge based on 
;; properties of a POPCO constraint
;; NOTE *THE-PERSON* must be set properly.
(defun fmt-guess-edge-update-cmds (constraint &optional (person *the-person*))
  (fmt-guess-edge-update-cmds-with-guess-nodes (personal-sym-to-guess-nodename (car constraint) person)
                                               (personal-sym-to-guess-nodename (cadr constraint) person)
                                               (cddr constraint)
                                               person))

;; Does the real work of formatting commands to update the appearance of a GUESS edge.
;; Helper function to be called either by:
;;      fmt-guess-edge-update-cmds [for which it does most of the work]
;; or:
;;      fmt-guess-edge-add-cmds.
;; Assumes that popco constraint data already converted to GUESS format.
(defun fmt-guess-edge-update-cmds-with-guess-nodes (node1 node2 weight person)
  (let ((edge-name (format nil "(~A-~A)" node1 node2))
        (color (cond ((> weight 0) "green") ; color of positive weight
                     ((< weight 0) "red")   ; color of negative weight
                     (t "gray")))
        (width (* *graphml-edge-multiplier* (abs weight))))
    (format nil 
            " ~A.weight = ~f ~% ~A.width = ~f ~% ~A.color = ~A ~%"
            edge-name weight
            edge-name width
            edge-name color)))


;; Format a string of commands to create a new GUESS edge based on properties of 
;; a POPCO constraint.
;; NOTE *THE-PERSON* must be set properly.
(defun fmt-guess-edge-add-cmds (constraint &optional (person *the-person*))
  (let ((node1 (personal-sym-to-guess-nodename (car constraint) person))
        (node2 (personal-sym-to-guess-nodename (cadr constraint) person)))
    (format nil "addEdge(~A,~A)~%" node1 node2)))

(defun fmt-guess-edge-remove-cmds (constraint &optional (person *the-person*))
  (let ((node1 (personal-sym-to-guess-nodename (car constraint) person))
        (node2 (personal-sym-to-guess-nodename (cadr constraint) person)))
    (format nil "remove(~A-~A)~%" node1 node2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTE: The following depends on sbcl-sockets.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Socket handling

;; Open a tcp connection to a GUESS instance on the same machine
;; This could be used to open multiple sockets to different instances of guess
(defun open-guess-socket (port)
  (tcp-connect "127.0.0.1" port))

;; Open the standard guess socket to a single instance of GUESS
(defun open-guess-socket-for-person (person &optional (port *first-port*))
  (let ((socket (open-guess-socket port))) ; DO I NEED TO DO ERROR HANDLING HERE?
    (set-guess-socket person socket)
    socket))

(defun start-report-to-guess (person &optional (port *first-port*))
  (let ((socket (open-guess-socket-for-person person port)))
    (update-guess-meta-for-person person)
    (send-guess-cmd (format nil "centerAfterLayout(false)~%") socket)
    (send-guess-cmd (format nil "setSynchronous(true)~%") socket)))

; cf. report-persons-to-guess in popco.lisp

; side-effecting the index is clearer here than a do-loop or recursion
(defun start-reports-to-guess (&optional (persons *persons-reporting-to-guess*))
  (let ((i -1))
    (mapc #'(lambda (person) (start-report-to-guess person (+ *first-port* (incf i))))
          persons)))

; for comparison, here are other versions:

; loop version
;(defun start-reports-to-guess (&optional (persons *persons-reporting-to-guess*))
;  (do ((i 0 (1+ i))
;       (pers persons (cdr pers)))
;      ((null persons) t)
;    (start-report-to-guess (car persons) (+ *first-port* i))))

; recursive version
;(defun start-reports-to-guess (&optional (persons *persons-reporting-to-guess*))
;  (start-reports-to-guess-aux persons 0))
;
;(defun start-reports-to-guess-aux (persons port-increment &optional (first-port *first-port*))
;  (cond ((null persons) t)
;        ((start-report-to-guess (car persons) (+ *first-port* port-increment))
;         (start-reports-to-guess-aux (cdr persons) (1+ port-increment)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General-purpose tcp send function wrapper used below

(defun send-guess-cmd (guess-cmd-string socket)
  (if socket
    (tcp-print-raw socket guess-cmd-string)
    (format t "~%send-guess-cmd: Connection to GUESS has not been established.~%")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adding nodes

(defun add-guess-nodes-for-persons (persons)
  (mapc #'add-guess-nodes-for-person persons))

(defun add-guess-nodes-for-person (person)
  (let ((socket (get-guess-socket person)))
    ; next two mapc's are identical except for the input list
    (when *do-report-analogy-nets-to-guess*
      (mapc #'(lambda (unit) (send-guess-cmd (fmt-guess-node-add-cmds unit person) socket))
            (get person 'newly-added-map-units)))
    (mapc #'(lambda (unit) (send-guess-cmd (fmt-guess-node-add-cmds unit person) socket))
          (get person 'newly-added-propn-units))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Updating nodes

(defun update-guess-nodes-for-persons (persons)
  (mapc #'update-guess-nodes-for-person persons))

(defun update-guess-nodes-for-person (person)
  (let ((socket (get-guess-socket person)))
    (mapc #'(lambda (unit) (send-guess-cmd (fmt-guess-node-update-cmds unit person) socket))
          (if *do-report-analogy-nets-to-guess*
            (get person 'all-units)
            (remove-if-not #'propn? (get person 'all-units))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adding edges

(defun add-guess-edges-for-persons (persons)
  (mapc #'add-guess-edges-for-person persons))

(defun add-guess-edges-for-person (person)
  (let ((socket (get-guess-socket person)))
    (mapc 
      #'(lambda (constraint) 
          ;(format t "Sending to GUESS: ~S~%" constraint) ; DEBUG
          (send-guess-cmd (fmt-guess-edge-add-cmds constraint person) 
                                             socket))
      (if *do-report-analogy-nets-to-guess*
        (get person 'newly-added-constraints)
        (remove-if-not #'constraint-has-propn (get person 'newly-added-constraints))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Removing edges

(defun remove-guess-edges-for-persons (persons)
  (mapc #'remove-guess-edges-for-person persons))

(defun remove-guess-edges-for-person (person)
  (let ((socket (get-guess-socket person)))
    (mapc 
      #'(lambda (constraint) (send-guess-cmd (fmt-guess-edge-remove-cmds constraint person) 
                                             socket))
        (get person 'newly-removed-constraints)))) ; ok to remove non-existent edges--GUESS won't complain

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Updating edges

; Note this process involves recreating the dotted triple representation of constraints, 
; which can be expensive, every time it's run, i.e. every pop-tick.  
; There may be a more efficient method, but the constraints read must be kept up 
; to date since weights can change.  However, we only recreate the dotted triples
; for the proposition network for those individuals that we're reporting.

(defun update-guess-edges-for-persons (persons)
  (mapc #'update-guess-edges-for-person persons))

(defun update-guess-edges-for-person (person)
  (let ((socket (get-guess-socket person)))
    (if *do-report-analogy-nets-to-guess*
      (mapc #'(lambda (constraint) (send-guess-cmd (fmt-guess-edge-update-cmds constraint person) socket))
            (remove-if #'constraint-has-propn (get person 'newly-added-constraints)))) ; skip propn constraints; next line will get them
    (mapc #'(lambda (constraint) (send-guess-cmd (fmt-guess-edge-update-cmds constraint person) socket))
          (list-constraints (get person 'all-propositions)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Update other info, e.g. displayed pop-tick number

(defun update-guess-meta-for-persons (persons)
  (mapc #'update-guess-meta-for-person persons))

(defun update-guess-meta-for-person (person)
  (let ((socket (get-guess-socket person)))
    (when (or 
            (get person 'newly-added-constraints)
            (get person 'newly-added-map-units)
            (get person 'newly-added-propn-units))
      (send-guess-cmd (format nil *guess-layout-commands*) socket) ; before this, setSynchronous(true) and maybe centerAfterLayout(false).
      (send-guess-cmd (format nil "(name<>\"\").visible=true~%") socket)) ; make new nodes and edges visible. [What's in parens gets all nodes.]
    (when (= 0 (mod *pop-tick* *extra-meta-interval*)) ; needs to be outside the preceding when in case there was an update on previous pop-ticks
      (send-guess-cmd (format nil *extra-meta-commands*) socket))
    (send-guess-cmd
      (format nil "telnetCommandServer.myLabel.setText(\"~A, ~A ~S\")" person *pop-tick-label* *pop-tick*)
      socket)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Removing nodes
;; current unused, but could allow re-initing of a graph
;
;(defun remove-guess-node (unit person)
;  (let ((socket (get-guess-socket person)))
;    (if socket
;      (tcp-print-raw socket (fmt-guess-node-remove-cmds unit person))
;      (format t "~%remove-guess-node: Connection to GUESS has not been established.~%"))))
;
;(defun remove-guess-nodes (units person)
;  (mapc #'(lambda (unit) (remove-guess-node unit person))
;        units))
;
;(defun remove-guess-nodes-for-person (person)
;  (remove-guess-nodes 
;    (get person 'all-units)
;    person))
;
;(defun remove-guess-nodes-for-persons (&optional (persons *persons-reporting-to-guess*))
;  (mapc #'remove-guess-nodes-for-person persons))

