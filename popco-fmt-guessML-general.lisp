;; popco-fmt-guessML-general.lisp
;; Copyright (c) 2012 by Marshall Abrams
;; May be distributed only with permission from the author.
;;
;; Data-formatting functions for output to GUESS via GraphML format files:
;; This file contains general-purpose functions for output to GUESS.
;; Also need to load at least one popco-fmt-guess-specific*.lisp to
;;  specify the appearance of nodes and edges.
;; Copyright (c) 2011 by Marshall Abrams
;; May be distributed only with permission from the author.

;;----------------------------------------------------------
;; POPCO/COHERE->GRAPHML-FORMAT functions
;; For displaying constraint networks in GUESS, NetworkWorkBench, etc.
;; NOTE: You must also load popco-guess-fmtN.lisp where N is 1, 2, etc.
;; Those files output GRAPHML code that represents POPCO data using a 
;; particular color/shape/etc. scheme, whereas code in this file is generic.

(defvar *deprecated-guess-map-string* "_maps_") ; string that will replace the "=" in ACME map unit names when converting unit names for GUESS:
(defvar *deprecated-guess-map-string-chars* (coerce *deprecated-guess-map-string* 'list)) ; list of characters corresponding to same string:
(defvar *deprecated-guess-gt-string* "gt")
(defvar *deprecated-guess-gt-string-chars* (coerce *deprecated-guess-gt-string* 'list))

; PERSONAL-SYM-TO-GUESS-NODENAME 
; Convert a POPCO personal sym to a string suitable for use as a node name
; in GUESS, i.e. suitable to be the name of a Python variable.  In particular
; dash and "=" are illegal, but underscore is OK.
; *THE-PERSON* MUST BE SET PROPERLY unless optional argument is used
(defun personal-sym-to-guess-nodename (sym &optional (person *the-person*))
  (generic-sym-to-guess-nodename (maybe-depersonalize-sym sym person)))

; GENERIC-SYM-TO-GUESS-NODENAME 
; Convert a POPCO generic sym to a string suitable for use as a node name
; in GUESS, i.e. suitable to be the name of a Python variable.  In particular
; dash and "=" are illegal, but underscore is OK.
; [Function works by expanding symbol name string into a list of characters to
; allow arbitrary substitutions, so we can substitute any number of characters
; for the equal sign.  Then we flatten out the list of chars we substituted
; Probably there's a more elegant, efficient way to do this--and it can make
; a noticeble difference in speed of the guess-file creation functions--but 
; this function doesn't need to be efficient as long as GUESS files are only
; created on an ad-hoc basis.]
(defun generic-sym-to-guess-nodename (sym)
  (cook-sym-name-for-others (symbol-name sym)))

; old version of preceding
(defun deprecated-generic-sym-to-guess-nodename (sym)
  (coerce 
    (flatten
      (substitute *deprecated-guess-gt-string-chars* #\>
                  (substitute *deprecated-guess-map-string-chars* #\=
                              (substitute #\_ #\- 
                                          (coerce 
                                            (symbol-name sym)
                                            'list)))))
    'string))


;; PERSONAL-SYMS-TO-GUESS-MAP-NODENAME 
;; Given two Lisp symbols which should normally be proposition nodes,
;; produce the corresponding map node, if it exists.
;; Tries constructing a map node from arguments in both orders
(defun personal-syms-to-guess-map-nodename (sym1 sym2)
  (or
    (personal-syms-to-poss-guess-map-nodename sym1 sym2)   ; nil if sym1=sym2 isn't a map node; guess node string if it is
    (personal-syms-to-poss-guess-map-nodename sym2 sym1))) ; ditto

(defun personal-syms-to-poss-guess-map-nodename (sym1 sym2)
  (let ((poss-map-node (catname sym1 sym2)))
    (if (is-acme-unit poss-map-node)
      (personal-sym-to-guess-nodename poss-map-node)
      nil)))

(defun graphml-header () 
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\"  
    xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
    xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns
    http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">
	<graph id=\"popco-graph\" edgedefault=\"undirected\">
")

(defun graphml-footer ()
  "	</graph>
</graphml>")

; *THE-PERSON* MUST BE SET PROPERLY.
(defun graphml-nodes (units)
  (apply #'concatenate 'string (mapcar #'graphml-node units)))

; *THE-PERSON* MUST BE SET PROPERLY.
(defun graphml-edges (constraints)
  (apply #'concatenate 'string (mapcar #'graphml-constraint-edge constraints)))

; WRITE-GRAPHML-GUESS-FILE
; By default the output file will include the SPECIAL node which represents environmental influence in Thagard's
; system.  The keyword argument in the example prevents the special and salient nodes from being included; if there its
; involved in no constraints, including it in graphs is usually undesirable.  (Removing the special node when
; it's included in an edge is probably a bad idea.)
; Example usage:
; (write-graphml-guess-file "yo4.graphml" (get 'polly 'all-units) (get 'polly 'all-constraints) :include-special-units nil)
;
; *THE-PERSON* MUST BE SET PROPERLY.
(defun write-graphml-guess-file (filename person units constraints
                                  &key (include-special-units t) (show-names nil) (names-inside nil)
                                       (include-graph-label-node nil))
    (with-open-file (outstream filename :direction :output :if-exists :rename :if-does-not-exist :create)
      (princ (graphml-header) outstream)
      (princ (graphml-guess-node-keys :show-names show-names) outstream)
      (princ (graphml-guess-edge-keys) outstream)
      (when include-graph-label-node
        (princ (default-graph-label-node-graphml person) outstream))
      (when include-special-units
        (when (find-if #'constraint-has-special constraints)
          (princ (graphml-special-node) outstream))
        ;(when (find-if #'constraint-has-salient constraints)
          (princ (graphml-salient-node) outstream))
        ;)
      (princ (graphml-nodes units) outstream)
      (princ (graphml-edges constraints) outstream)
      (princ (graphml-footer) outstream))
      filename)

;; note graph-label must be a string, not e.g. a symbol
(defun write-person-graphml-guess-file (filename person 
                                        &key (include-special-units t) (show-names nil) (names-inside nil) (graph-label person)
                                             (do-report-analogy-nets *do-report-analogy-nets-to-guess*)
                                             (include-graph-label-node nil))
  (setf *the-person* person)
  (write-graphml-guess-file
    filename
    person
    (if *do-report-analogy-nets-to-guess*
      (get person 'all-units)
      (remove-if-not #'propn? (get person 'all-units)))
    (if *do-report-analogy-nets-to-guess*
      (get person 'all-constraints)
      (remove-if-not #'constraint-has-propn (get person 'all-constraints)))
    :include-special-units include-special-units
    :show-names show-names
    :include-graph-label-node include-graph-label-node))

(defun write-person-graph (filename person
                           &key (include-special-units t) (show-names nil) (names-inside nil) 
                                (recreate-constraints t) (graph-label (default-graph-label person))
                                (include-graph-label-node nil))
  (when recreate-constraints (record-person-constraints person))
  (write-person-graphml-guess-file filename person
                                   :include-special-units include-special-units
                                   :show-names show-names 
                                   :graph-label graph-label
                                   :include-graph-label-node include-graph-label-node))

;; WRITE-PERSON-GRAPHS
;; Write GUESS GraphML files for all persons in population if a population symbol
;; is passed, or all persons in list that's passed instead.
;; i.e. 
;;     (write-person-graphs "folks_" 'folks)
;; has the same effect as
;;     (write-person-graphs "folks_" (get 'folks 'members)) .
;; 
;; It's bad form to mix &optional and &key params in the same defun, as here.... 
;; NOTE: Calls must include the optional args if *any* keyword args are given.
(defun write-person-graphs (basename &optional (persons-or-pop *the-population*) 
                                     &key (include-special-units t) (show-names nil) (recreate-constraints t) (include-graph-label-nodes nil))
  (let ((persons (if (listp persons-or-pop)
                   persons-or-pop
                   (get persons-or-pop 'members))))
    (mapc #'(lambda (pers)
              (format t "~S~%"
                      (write-person-graph                               ; write-person-graph returns the filename
                        (concatenate 'string basename (string-downcase (symbol-name pers)) ".graphml")
                        pers 
                        :include-special-units include-special-units 
                        :show-names show-names 
                        :recreate-constraints :recreate-constraints
                        :include-graph-label-node include-graph-label-nodes))) ; note plural/singular switch
          persons)))



; This is supposed to put all persons in one graph file
; Doesn't work yet
;(defun write-persons-graphml-guess-file (filename persons 
;                                         &key (include-special-units t) (show-names nil) (include-graph-label-node nil))
;  (write-graphml-guess-file
;    filename
;    (append (mapcar #'all-units persons))
;    (append (mapcar #'all-constraints persons))
;    :include-special-units include-special-units
;    :show-names show-names
;    :include-graph-label-node include-graph-label-node))

