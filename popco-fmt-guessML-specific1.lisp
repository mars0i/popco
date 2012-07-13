;; popco-fmt-guess-specific1.lisp
;; Copyright (c) 2012 by Marshall Abrams
;; May be distributed only with permission from the author.
;; 
;;----------------------------------------------------------
;; POPCO/COHERE->GRAPHML-FORMAT functions for use by popco-guessML-general.lisp
;;----------------------------------------------------------
;; Data-formatting functions for output to GUESS via GraphML format files:
;; This file specifies the appearance of nodes and edges.
;; Also need to load popco-fmt-guessML-general.lisp, which defines general-purpose
;;  functions for output to GUESS.
;; 
;; This version produces networks in which:
;;	nodes (units) are round
;;	names of units are not displayed (but can be gotten by mouseover)
;; 	color of a node represents positive (orange) and negative (blue) activation
;;	diameter of a node represents absolute value of activation
;;	color of a link represents positive (green) and negative (red) weight
;;	thickness of a link represents absolute value of weight
;; Note that relationships between map nodes and proposition links is not represented
;; explicitly.
;; For displaying constraint networks in GUESS, NetworkWorkBench, etc.

;; 4/23/2012:
;; Made edges and nodes invisible by default, but explicitly set to visible in each one.
;; This allows dynamic edge creation commands to create items as invisible before setting
;; their attributes and modifying the layout, and then make them visible.

(defvar *other-node-pos-color* "white")
(defvar *other-node-neg-color* "black")
(defvar *propn-node-pos-color* "yellow")
;(defvar *propn-node-neg-color* "royalblue")
(defvar *propn-node-neg-color* "navyblue")

(defun graphml-guess-node-keys (&key (show-names t) (names-inside nil))
  (format nil
"		<key id=\"guess-node-color\" for=\"node\" attr.name=\"color\" attr.type=\"string\">
			<default>lightgray</default>
		</key>
		<key id=\"guess-node-height\" for=\"node\" attr.name=\"height\" attr.type=\"double\">
			<default>10</default>
		</key>
		<key id=\"guess-node-width\" for=\"node\" attr.name=\"width\" attr.type=\"double\">
			<default>10</default>
		</key>
                <!-- The next two keys should be coordinated.... -->
		<key id=\"guess-node-label-visible\" for=\"node\" attr.name=\"labelvisible\" attr.type=\"boolean\">
			<default>~A</default>
		</key>
		<key id=\"guess-node-style\" for=\"node\" attr.name=\"style\" attr.type=\"int\">
			<default>~S</default>
		</key>
		<!-- Normally, the node label will be inherited from the node name, but this key allows arbitrary labels without changing the node name. -->
		<key id=\"guess-node-label\" for=\"node\" attr.name=\"label\" attr.type=\"string\">
			<!-- No default value is needed.  Default will be the usual inherit-from-nodename behavior. -->
		</key>
                <!-- possible styles: rectangle: 1, ellipse: 2, rounded rectangle: 3, text in rectangle: 4,
                     text in ellipse: 5, text in rounded rectangle: 6, image: 7 -->
		<!-- For map nodes, 1st and 2nd elements of the CONCERNS property in ACME: -->
		<key id=\"guess-node-visible\" for=\"node\" attr.name=\"visible\" attr.type=\"boolean\">
			<default>false</default>  <!-- Set invisible by default.  Elsewhere explicitly set each to be visible. -->
		</key>
		<key id=\"guess-node-concerns1\" for=\"node\" attr.name=\"concerns1\" attr.type=\"string\"/>
		<key id=\"guess-node-concerns2\" for=\"node\" attr.name=\"concerns2\" attr.type=\"string\"/>
		<!-- ACME unit's activation value (which determines GUESS node size): -->
		<key id=\"guess-node-activation\" for=\"node\" attr.name=\"activation\" attr.type=\"string\"/>
"   (if show-names "true" "false")
    (if names-inside 6 2)))

(defun graphml-guess-edge-keys ()
"		<key id=\"guess-edge-color\" for=\"edge\" attr.name=\"color\" attr.type=\"string\">
			<default>yellow</default>
		</key>
		<!-- Getting edge labels to display is a bit flaky: -->
		<key id=\"guess-edge-label-visible\" for=\"edge\" attr.name=\"labelvisible\" attr.type=\"boolean\">
			<default>false</default>
		</key>
		<key id=\"guess-edge-label-color\" for=\"edge\" attr.name=\"labelcolor\" attr.type=\"boolean\">
			<default>white</default>
		</key>
		<key id=\"guess-edge-visible\" for=\"edge\" attr.name=\"visible\" attr.type=\"boolean\">
			<default>false</default>  <!-- Set invisible by default.  Elsewhere explicitly set each to be visible. -->
		</key>
		<key id=\"guess-edge-width\" for=\"edge\" attr.name=\"width\" attr.type=\"double\"/>
		<key id=\"guess-edge-weight\" for=\"edge\" attr.name=\"weight\" attr.type=\"double\"/>
		<!-- For links between propositions, the edge weight is a function of the activation of this node: -->
		<key id=\"guess-edge-weight-source\" for=\"edge\" attr.name=\"weightsource\" attr.type=\"string\"/>
")

(defun graphml-special-node ()
"		<node id=\"SPECIAL\">
		        <data key=\"guess-node-visible\">true</data> <!-- override default set above -->
	        	<data key=\"guess-node-color\">purple</data>
	        	<data key=\"guess-node-height\">20</data>
	        	<data key=\"guess-node-width\">20</data>
                </node>
")

(defun graphml-salient-node ()
"		<node id=\"SALIENT\">
		        <data key=\"guess-node-visible\">true</data> <!-- override default set above -->
	        	<data key=\"guess-node-color\">purple</data>
	        	<data key=\"guess-node-height\">20</data>
	        	<data key=\"guess-node-width\">20</data>
                </node>
")

(defun guess-node-color (unit)
  (cond
    ((or (propn? unit) (propn-map? unit))
     (if (plusp (activation unit)) 
       *propn-node-pos-color* 
       *propn-node-neg-color*))
    (t 
      (if (plusp (activation unit)) 
        *other-node-pos-color* 
        *other-node-neg-color*)))) ; maybe later split into pred and obj maps

(defun guess-node-size (unit)
  (+ *graphml-node-floor* 
     (abs (* (activation unit) 
             *graphml-node-multiplier*))))

; By default I'm taking the person name out of the node name.
; Maybe there will be a reason to make this optional this later?
; *THE-PERSON* MUST BE SET PROPERLY.
(defun graphml-node (unit)
  (let* ((activn (activation unit))
         (color (guess-node-color unit))
         (size (guess-node-size unit))
         (concerns1 (first (get unit 'concerns)))    ; = nil if concerns property is empty or non-existent
         (concerns2 (second (get unit 'concerns))))  ; ditto
    (format nil 
"               <node id=~S>
		        <data key=\"guess-node-visible\">true</data> <!-- override default set above -->
			<data key=\"guess-node-color\">~A</data>
			<data key=\"guess-node-height\">~f</data>
			<data key=\"guess-node-width\">~f</data>
			<data key=\"guess-node-activation\">~f</data>
			<data key=\"guess-node-concerns1\">~A</data>
			<data key=\"guess-node-concerns2\">~A</data>
                </node>
"
            (personal-sym-to-guess-nodename unit)
            color 
            size 
            size
            activn
            (if concerns1 (personal-sym-to-guess-nodename concerns1) "")
            (if concerns2 (personal-sym-to-guess-nodename concerns2) ""))))


; *THE-PERSON* MUST BE SET PROPERLY.
(defun graphml-constraint-edge (constraint)
  (let* ((unit1 (car constraint))
         (unit2 (cadr constraint))
         (unit1-name (personal-sym-to-guess-nodename unit1))
         (unit2-name (personal-sym-to-guess-nodename unit2))
         (weight (cddr constraint)))
    (format nil
"		<edge source=~S target=~S>
		        <data key=\"guess-edge-visible\">true</data> <!-- override default set above -->
		        <data key=\"guess-edge-color\">~A</data>
		        <data key=\"guess-edge-width\">~f</data>
		        <data key=\"guess-edge-weight\">~f</data>
			<data key=\"guess-edge-weight-source\">~A</data>
		</edge>
"
            unit1-name
            unit2-name
            (cond ((> weight 0) "green")
                  ((< weight 0) "red")
                  (t "gray"))
            (* *graphml-edge-multiplier* (abs weight))
            weight
	    (or (personal-syms-to-guess-map-nodename 
                  (personal-to-generic-sym unit1) 
                  (personal-to-generic-sym unit2))
                ""))))

(defun default-graph-label (person) 
  (format nil "~S" person))

;(defun default-graph-label (person) 
;  (format nil "~S gen ~S" person *pop-tick*))

;(defun default-graph-label (person) 
;  (format nil "~S_gen_~S_pexc_~f_pinh_~f_max_~f" person *pop-tick* *propn-excit-weight* *propn-inhib-weight* +acme-max-weight+))

(defun default-graph-label-node-graphml (person)
  (format nil
"		<node id=~S>
		        <data key=\"guess-node-visible\">true</data> <!-- override default -->
	        	<data key=\"guess-node-color\">white</data>
	        	<data key=\"guess-node-label-visible\">true</data>
	        	<data key=\"guess-node-style\">4</data>
                </node>
                "
          (default-graph-label person)))
