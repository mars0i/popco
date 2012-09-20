;; UI FOR POPCO 2012
;; For NetLogo 5.0.2 and up

;; NOTE this version uses a very large "world" i.e. the box the agents appear in.
;; You'll probably need to increase the Java heap size to do this.
;; e.g. I found the default 1024M to be too small, but 1536M large enough.
;; See http://ccl.northwestern.edu/netlogo/docs/faq.html#howbig
;; for instructions for changing the heap size in different OSes.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTE THIS VERSION FOR USE WITH HACKED VERSION OF NETLOGO IN WHICH ;;
;; eof() in DefaultFileManager.java has been modified by commenting  ;;
;; out the if so that the eof state can be undone if a file receives ;;
;; new data.  The last version designed for normal NetLogo 5.0.1 was ;;
;; popco43.nlogo.                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This version of this experimental code lets the go forever loop wait
;; to see if more data arrives.  It works!

;; NOTE:
;; Formerly, this program was able to display components of propositions
;; (predicates and object arguments) as well as propositions.  This
;; turned out not to be very useful.  For a while, I simply didn't use this
;; functionality.  Then I commented out all of the code.  At this point,
;; I'm deleting the commented code to simplify things.  However, at this
;; point the input file still contains data about predicates and objects,
;; but that data is ignored.  That means that the data type for propositions
;; is LangElem, which also could, in theory, be a predicate or an object.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

extensions [table]  ; hashtable/lookup table functions, and replacement for file-at-end?

; convention: breeds are studly-capped
breed [Persons Person]
breed [LangElems LangElem]                   ; a proposition or concept (predicate or object)
undirected-link-breed [MapLinks MapLink]     ; links representing mappings between propns, etc.
directed-link-breed [Utterances Utterance]   ; communication between two Persons

globals
[
  base-thickness
  scale-thickness
  remaining-colors
  ;default-lang-elem-size ; WILL BE OBSOLETE, but still useful for non-proposition LangElems
  propn-size-multiplier ; scaling factor for sizing of propositions
  propn-size-min ; minimum size for propositions
  propn-max-size ; = propn-size-min + propn-size-multiplier * 1 [the last term is the max abs activation]
  
  person-size
  person-color
  
  data-is-from-file ; true if we are reading our data from a file, false if we are getting it from internal processes (e.g. abcl)
  
  population-data-file ; gives a series of internal states of each person in pop at a given time/tick
  ;SYNTAX: list-of-persons list-of-conversations list-of-persons list-of-conversations ...
  ; person: [name [proposition ...] [predicate ...] [object ...] [maplink ...]]
  ; maplink: [proposition proposition activation]
  ; conversation: [is-new-to-listener proposition speaker listener]

  population-data ; will hold data from entire file
  population-data-ticks ; number of ticks (state, conversations pairs)
  
  all-proposition-names ; will contain a list of all propositions possible in the model run
  
  person-list ; only used inside the go procedure, but defined as a global so that we can check value from previous run
  
  ; CONSTANTS for accessing data lists coming over from Lisp--each is the
  ; index of a "field" in a list of data items for a single person.
  ; (or should I implement association lists in netlogo? less bug-prone, but slower)
  PERSON-NAME-idx
  PROPOSITIONS-idx
  PREDICATES-idx
  OBJECTS-idx
  MAPLINKS-idx
  PROPN-ACTIVATIONS-idx ; activations for propositions
  
  propn-cat-prefixes ; list of prefixes indicating propn categories
  propn-cat-descs ; list of corresponding descriptions
  propn-avg-plots ; list of names of plots for average activations in each proposition category
  propn-activn-plots ; names of plots for raw activations in each proposition category
  ;propn-cat-desc-x-centers ; x coordinates in pixels of location to add description of each plot
  ;propn-cat-desc-y-tops ; y coordinates of same
  ; preceding 5 lists should be of same length
   
  read-wait-secs ; number of seconds to wait between attempts to read input dat
  
  layout-radial-uncentered-backoff
  
  color-sequence ; list of allowed colors for e.g. LangElems
  
  canonical-propn-coords-tbl ; table (using tables extension) that maps proposition names to coords relative to 0,0
  
  default-LangElem-shape ; default shape for LangElems
  propn-listen-shape ; shape of a proposition LangElem when it's on the listener end of an utterance (because link arrowheads are too small)
  propn-listen-size ; size to make proposition LangElem when at listener end of an utterance
  
  status-message-flag  ; when this appears in input data, what follows is a status string
]

LangElems-own
[
 elem-name         ; identify of lang elem, i.e. which particular proposition or concept
 elem-person-name  ; whose mentalese is this an element of?
 propn-activation  ; THIS FIELD IS ONLY FOR PROPOSITIONS, which have standalone nodes in POPCO 
 is-heard ; temporary flag set during ticks in which this proposition was uttered to this elem's person
 current-default-size  ; Allows temporarily changing size, later reverting to whatever is stored here.
]


Persons-own
[
  person-name
]

MapLinks-own
[
  map-activation ; fundamental property of a mapping, other than the two elements mapped
]


to setup
  clear-all
  
  ; These lists must be kept in sync with names and locations of plots.
  ; (The odd ordering is so that the orientation of the graphs in relation to each other
  ; will correspond to the ordering of the proposition categories around each person's circle
  ; if there are four categories with roughly equal numbers of propositions.)
  set propn-avg-plots ["domain 4 avgs" "domain 3 avgs" "domain 1 avgs" "domain 2 avgs"]
  set propn-activn-plots ["domain 4 activations" "domain 3 activations" "domain 1 activations" "domain 2 activations"]
  ;set propn-cat-desc-x-centers [415 140 140 415]
  ;set propn-cat-desc-y-tops [570 570 285 285]
 
  ; indexes into lists in input data coming over from the Lisp side
  set PERSON-NAME-idx 0
  set PROPOSITIONS-idx 1
  set PREDICATES-idx 2
  set OBJECTS-idx 3
  set MAPLINKS-idx 4
  set PROPN-ACTIVATIONS-idx 5
  ;set EOF-FLAG false
  
  ; type specifiers for LangElems (there's no breed inheritance, so distinguish LangElems this way)
  ;set PROPOSITION-type 0
  ;set PREDICATE-type 1
  ;set OBJECT-type 2
  
  set read-wait-secs 1 ; any value over 0 seems to have same effect on cpu usage (no wait uses a bit more cpu)
  
  ;set usual-layout-iterations 300
  set layout-radial-uncentered-backoff 10
  
  ; List the non-gray shades in middling levels of light/dark;
  ; allows choosing a different color for each whatever.
  set color-sequence [15 85 55 125 42 33 115 125 135 35 25 75 95 105
                      16 26 36 56 66 76 86 96 106 116 126 136
                      14 24 34 124 54 64 74 84 94 104 114 134
                      17 37 67 77 87 97 107 117 127 137
                      13 23 43 53 63 73 83 93 103 113 123 133
                      18 28 38 48 58 68 78 88 98 108 118 128 138
                      12 22 32 52 62 72 82 92 102 112 122 132]
  ;set color-sequence shuffle ( filter [(? mod 10 >= 3) and (? mod 10 <= 7)]  (n-values 140 [?]) ) ; from Voronoi.nlogo
  set remaining-colors color-sequence

  set default-LangElem-shape "circle"
  set-default-shape LangElems default-LangElem-shape
  set-default-shape Persons "circle"
  set-default-shape Utterances "dashedBigarrow"
  set propn-listen-shape "circle 2" ; "target"
  set propn-listen-size 7
  ;print (word "in setup: propn-listen-size = " propn-listen-size) ; DEBUG
  ;set default-lang-elem-size 3  ; THIS WILL BECOME OBSOLETE
  set propn-size-min 2
  set propn-size-multiplier 3
  set propn-max-size (propn-size-min + (propn-size-multiplier * 1))
  set person-size 5
  set person-color black
  set base-thickness 0
  set scale-thickness 1.5
  
  set status-message-flag "status:" ; when this appears in input data, what follows is a status string
  
  
  ;;; load data to be displayed ;;;
  
  set population-data []
  
  if (open-population-data) = false [stop] ; error reporting done in just-called procedure
  
  ;;; display initial state of population from input data ;;;
  
  if (set-initial-population-config) = false [stop] ; error reporting done in just-called procedure 
  ; note that Persons, LangElems, OwnerLinks are created hidden, and won't appear until asked

  setup-layout
  ask turtles [show-turtle] ; This seems necessary to get everything displayed,
  ask links [show-link]     ;  but I don't understand why calling update-display
  update-display            ;  isn't enough on its own, given the code in it.
  setup-my-plots
  clear-output
  reset-timer
  reset-ticks
end
;; end of to setup


to go
  ;print date-and-time ; DEBUG
  
  popup-elem-info
  
  set person-list get-next-state  ; try to get population state data

  ; This multi-level if-else block extends to the end of the go routine.
  if-else person-list = false [ 
    ; if we got no data:
    if-else stop-if-no-data [
      output-print (word timer " seconds since setup was run.")
      stop
    ][
      ; note that if we didn't get any state data, there's no conversation data either
      wait read-wait-secs ; wait a bit before trying to get more data
    ]
    ; now back to beginning of go procedure. note we don't increment the tick.
  ][ 
    ; if we did get data, it's either a status message or a new population state:
    if-else (status-message-flag = (first person-list)) [ ; if not really a person-list, but a status update
      clear-output
      output-print (item 1 person-list)
      ; now back to beginning of the go procedure. note we don't increment the tick.
    ][ 
      ; if we really did get a list of persons and their internal states, then:
      if not update-population-state person-list [   ; use the data to modify the state of the pop on screen, etc.
        error "update-population-state returned false; this should not happen."
      ]
  
      if not do-conversations get-next-convs [  ; try to get new data on conversations, and display it
        error "do-conversations returned false; this should not happen."
      ]
  
      ask turtles [show-turtle] ; This seems necessary to get everything displayed,
      ask links [show-link]     ; but I don't understand why calling update-display
      update-display            ; isn't enough on its own, given the code in it.
      write-to-my-plots
      tick ; note that we only update the tick number in this if fork, i.e. if we got new state data
    ]
  ]
end

;to profile
;  profiler:start         ;; start profiling
;  repeat 200 [ go ]       ;; run something you want to measure
;  profiler:stop          ;; stop profiling
;  print profiler:report  ;; view the results
;  profiler:reset         ;; clear the data
;end

to finish
  file-close-all
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data Input Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report open-population-data
  file-close-all  ; make sure no open files hanging around from previous runs
  
  ; read data on persons in population, create persons:
  set population-data-file user-file
  if population-data-file = false [
    print "Failed to open a population data file."
    report false
  ]
  
  file-open population-data-file
  set data-is-from-file true
  
  report true
end

to-report get-propn-cat-prefixes
  if-else data-is-from-file [
    report (file-read)
  ][
    error "Getting data from internal processes is not yet implemented."
  ]
end

to-report get-propn-cat-descs
  if-else data-is-from-file [
    report (file-read)
  ][
    error "Getting data from internal processes is not yet implemented."
  ]
end

to-report get-initial-state
  if-else data-is-from-file [
    report (file-read)
  ][
    error "Getting data from internal processes is not yet implemented."
  ]
end

; Wrapper function for whatever operation gets the next population state data
; Returns false if at end of file, or data list otherwise.
to-report get-next-state
  if-else data-is-from-file [
    if-else file-at-end? [
      report false
    ][
      report file-read
    ]
  ][
    error "Getting data from internal processes is not yet implemented."
  ]  
end

; wrapper function for whatever operation gets the next conversation data
to-report get-next-convs
  if-else data-is-from-file [
    report (file-read)  ; if we hit EOF here, the file is bad; let it error out.
  ][
    error "Getting data from internal processes is not yet implemented."
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to-report set-initial-population-config []
  let result 0 ; temporary dummy value
  let pop-initial-state 0 ; ditto

  ; we're still not sure whether the file that user loaded looks like a good data file
  carefully [
    set propn-cat-prefixes get-propn-cat-prefixes
    set propn-cat-descs get-propn-cat-descs

    if (length propn-cat-prefixes) > (length propn-avg-plots) [
      error "Too many proposition category prefixes for the number of plots we have."
    ]

    if (length propn-cat-prefixes) != (length propn-cat-descs) [
      error "Number of proposition category prefixes is not equal to number of propn category descriptions."
    ]
        
    set pop-initial-state get-initial-state
    set all-proposition-names (extract-proposition-names pop-initial-state)
    set result true
  ][
    print "In procedure set-initial-population-config:"
    print "The chosen population data file appears to have the wrong format."
    print "NetLogo's error message was: "
    print error-message
    report false
  ]

  foreach pop-initial-state [make-Person2 ?] ; note that Persons, LangElems, OwnerLinks are created hidden, and won't appear unless asked

  ; it seems we are allowed to put 'report' in second clause of 'carefully' but not the first, so we'll do it here:
  report result
end

to-report extract-proposition-names [pop-initial-state]
  report remove-duplicates                                     ; Remove dupes from ...
           reduce [sentence ?1 ?2]                             ; a list concatenated from ...
              map [item PROPOSITIONS-idx ?] pop-initial-state  ; a list of lists of proposition names ...
end                                                            ; extracted from a list of person-specifications.
; indentation seems easier to read than parens in NetLogo, even if comfortable with Lisp.

to setup-my-plots
  setup-avg-plot "key"; a dummy plot just to display the legend of person names
  
  ; Shorten our list of plots to the length of our prefix list--ignore the other plots.
  ; This will allow foreaching through them later.  (We checked that there weren't too many prefixes when we loaded them.)
  set propn-avg-plots (sublist propn-avg-plots 0 (length propn-cat-prefixes))
  foreach propn-avg-plots [
    setup-avg-plot ?
  ]
  
  set propn-activn-plots (sublist propn-activn-plots 0 (length propn-cat-prefixes))
  (foreach propn-activn-plots propn-cat-prefixes [
     setup-activn-plot ?1 ?2
  ])
end

; For plot of per-person summed activations: i.e. plot a sum all of  positive and
; negative activations in some category for each person
to setup-avg-plot [this-plot-name]
  let my-pen-mode 0   ; 0 line, 2 points, 1 bar
  set-current-plot this-plot-name
  ; create a pen for each person
  foreach sort-on [person-name] Persons [         ; we want them listed in alphabetical order
    let pers-name false
    ask ? [set pers-name person-name]
    let this-color next-color
    create-temporary-plot-pen pers-name
    set-plot-pen-color this-color
    set-plot-pen-mode my-pen-mode
  ]
  reset-colors
  ; pen to display average over all persons--this could be done in the chart object, but better to consolidate here:
  create-temporary-plot-pen "pop" ; population average
  set-plot-pen-mode my-pen-mode
  set-plot-pen-color black
end

to setup-activn-plot [this-plot-name prefix]
  let my-pen-mode 0   ; 0 line, 2 points, 1 bar
  set-current-plot this-plot-name
  let propn-names filter [name-has-prefix ? prefix] all-proposition-names 

  ; We create a pen for each person/propn, where propn is in category with prefix, 
  ; assigning the same person color to all propns from that person:
  foreach sort-on [person-name] Persons [         ; we want them listed in alphabetical order
    let pers-name false
    ask ? [set pers-name person-name]
    let this-color next-color
    
    foreach propn-names [
      create-temporary-plot-pen (word pers-name "-" ?)
      set-plot-pen-color this-color
      set-plot-pen-mode my-pen-mode
    ]
  ]
  reset-colors
  ; pen to display average over all persons--this could be done in the chart object, but better to consolidate here:
  create-temporary-plot-pen "pop" ; population average
  set-plot-pen-mode my-pen-mode
  set-plot-pen-color black
end

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

to write-to-my-plots
  (foreach propn-avg-plots propn-cat-prefixes [
     plot-avg-activns ?1 (propns-with-prefix ?2)
  ]) ; parens are necessary syntax voodoo
  
  (foreach propn-activn-plots propn-cat-prefixes [
     ;print (word "prefix: " ?2 " plot: " ?1) ; DEBUG
     plot-activns ?1 ?2
  ])
end

to plot-activns [this-plot-name prefix]
  ;let elems propns-with-prefix prefix
  let propn-names filter [name-has-prefix ? prefix] all-proposition-names 
  ; We use propn names with the prefix rather than LangElems because we want to plot a point for missing propns.
  ; Otherwise once a propn is added to a person, its graph will be behind the other graphs.
  
  set-current-plot this-plot-name

  ask Persons [
    ;print (word "person: " person-name) ; DEBUG
    let pers-name person-name ; for use inside LangElem blocks
    
    foreach propn-names [
      set-current-plot-pen (word person-name "-" ?)
      let elem-set (LangElems with [(elem-person-name = pers-name) and (elem-name = ?)]) ; an agentset of size 0 or 1
      if-else (any? elem-set) [
        ;write (word " " ?) ; DEBUG
        ask elem-set [plot propn-activation] ; one member in the agentset
      ][
        ;write (word "not-" ?) ; DEBUG
        plot 0
      ]
    ]
    print "" ; DEBUG
  ]
end

to plot-avg-activns [this-plot-name elems]
  let my-elems []
  set-current-plot this-plot-name
  ask Persons [
    set-current-plot-pen person-name
    set my-elems (elems with [elem-person-name = ([person-name] of myself)])
    if-else (count my-elems) > 0 [
      plot average-activation my-elems
    ][
      plot 0 ; need to plot something--otherwise the graphs will be out of sync
    ]        ;  i.e. we treat the average of zero activation values to be zero.
  ]
  set-current-plot-pen "pop" ; population average
  if-else (count elems) > 0 [
    plot average-activation elems
  ][
    plot 0
  ]
end

to-report average-activation [elems]
  report (sum [propn-activation] of elems)/(count elems)
end

to setup-layout
  make-canonical-propn-layout all-proposition-names
  layout-circle Persons person-circle-radius
  arrange-LangElems Persons
  ask Persons [set label person-name]
  ;ask OwnerLinks [hide-link]
end

to arrange-LangElems [pursins]
  ; we want to move each person's LangElems near the person.
  ask pursins [
    let pers-name person-name
    arrange-propns (LangElems with [elem-person-name = pers-name]) self   ; and elem-type = PROPOSITION-type
    ;arrange-concepts (LangElems with [elem-person-name = pers-name and elem-type != PROPOSITION-type]) self
  ]

end

to arrange-propns [propns pers]
  let pers-name ""
  ask pers [set pers-name person-name]
  
  ask LangElems with [elem-person-name = pers-name] [  ; and elem-type = PROPOSITION-type
    layout-propn-canonically self pers
  ]
end

to update-display
  ask LangElems [  ; with [elem-type = PROPOSITION-type]
    set-proposition-appearance self
  ]
  
  ask MapLinks [                      ; later split into map/inference vs propn-component links
    if map-activation > 0 [
        set color green
        set shape "default"
    ]
    if map-activation < 0 [
        set color red
        set shape "dashed"
    ]
    if map-activation = 0 [ ; rare case
        set color blue
        set shape "default"
    ]
    set thickness base-thickness + ((abs map-activation) * scale-thickness)
  ]

  ifelse show-Person-labels [
    ask Persons [show-turtle]
  ][
    ask Persons [hide-turtle] ; automatically suppresses display of labels
  ]
  
  ifelse show-MapLink-labels [
    ask MapLinks [set label map-activation]
  ] [
    ask MapLinks [set label ""]
  ]

  ifelse show-LangElem-Labels [
    ask LangElems [set label elem-name]
  ] [
    ask LangElems [set label ""]
  ]

  ask (MapLinks with [any? (both-ends with [hidden?])]) [hide-link] ; i.e. hide links with either turtle hidden
  ask (MapLinks with [all? both-ends [not hidden?]]) [show-link] ; i.e. show links with both turtles showing
  ; Notes:
  ; - any? tests emptiness of agentset created by applying both-ends to links satisfying bracket-created reporter.
  ; - all? applies bracket-created reporter to agentset created by applying both-ends to links.
  ; - note that hidden? is link's variable containing true or false
end

to set-proposition-appearance [propn-LangElem]
  ask propn-LangElem [
    if-else is-heard = true [
      ;print (word "[" size "," propn-listen-size "]" elem-person-name ": " elem-name) ; DEBUG
      set current-default-size size ; record current usual size
      set size propn-listen-size    ; set to special listener size
      set shape propn-listen-shape  ; and shape
    ][
      set shape default-LangElem-shape
      if (propn-activation > 0) [
        set color yellow
        set size propn-size-min + propn-size-multiplier * propn-activation
      ]
      if (propn-activation < 0) [
        set color blue
        set size propn-size-min + (propn-size-multiplier * (- propn-activation))      
      ]
      if (propn-activation = 0) [
        set color gray
        set size propn-size-min
      ]
    ]
  ]
end

; Passed an agentset of LangElems and a radius, generates a circle, centered on center of the world,
; of LangElems ordered by elem-name.  The first of the LangElems will be directly above the center of the world.
to layout-circle-LangElems-by-name [lang-elems radius]
  layout-circle (sort-by [ [elem-name] of ?1 < [elem-name] of ?2 ] lang-elems) radius
end

; We want an official layout for all propositions that might be in a person;
; each person will put those proposition it has in locations specified by the
; official layout, and other spots in that layout will be empty for that person.
; This procedure creates and stores this layout in the form of a hashtable
; with proposition names as keys, and xcor,ycor list pairs as values.
; The coords are relative to the center.  After specifying a proposition's
; layout, move it to the desired location. (Would it be faster to keep the dummy
; LangElems around, and use *them* as the hashtable?  i.e. just ask for them and
; get their xcor,ycor and transfer to a same-name proposition?)
to make-canonical-propn-layout [all-propn-names]
  
  ; in this version of this procedure,
  ; we'll use turtle-layout functions (or our own procedures defined in terms of them)
  ; on temporary proposition LangElems.  So:
  
  ; make a temporary LangElem for each possible proposition name in the dataset
  foreach all-propn-names [
    create-LangElems 1 [
      hide-turtle 
      ;set elem-type PROPOSITION-type
      set elem-name ?
      set elem-person-name false  ; something that no legitimate proposition could have
    ]
  ]
  
  ; now layout the temporary propositions in the desired shape
  layout-circle-LangElems-by-name (LangElems with [elem-person-name = false])  propositions-circle-radius  ; elem-type = PROPOSITION-type and 
  
  ; create, then fill the hashtable
  set canonical-propn-coords-tbl table:make
 
  ; as soon as we get the layout coordinates from these temporary propositions, they can die
  ask LangElems with [elem-person-name = false] [  ; elem-type = PROPOSITION-type and 
    table:put canonical-propn-coords-tbl elem-name (list xcor ycor)
    die
  ]
end

;to layout-propns-canonically [propns]
;  ask propns [
;    let coords table:get canonical-propn-coords-tbl elem-name
;    set xcor (item 0 coords)
;    set ycor (item 1 coords)
;  ]
;end

to layout-propn-canonically [propn pers]
  ; get coordinates of person
  let pers-xcor 0
  let pers-ycor 0
  ask pers [
    set pers-xcor xcor
    set pers-ycor ycor
  ]

  ; set coords of proposition, reflecting both offsets of the person, and this proposition's
  ; place in relation to other propositions in the canonical arrangement
  ask propn [
    let coords table:get canonical-propn-coords-tbl elem-name ; get canonical coords for this proposition
    set xcor (item 0 coords) + pers-xcor  ; set to canonical coord + person's offset
    set ycor (item 1 coords) + pers-ycor
  ]
end

to popup-elem-info
  let candidate min-one-of LangElems [distancexy mouse-xcor mouse-ycor]
  while [mouse-down?] [
    let cand-size false
    ask candidate [set cand-size size]
    if [distancexy mouse-xcor mouse-ycor] of candidate < cand-size [
      ask candidate [
        ;set label-color gray
        set label (word elem-name " " propn-activation)
      ]
      display
    ]
  ]
  ask candidate [set label ""]
  display
end

;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;

to go-movie
  show (sentence "running movie for " population-data-ticks " ticks")
  movie-start "popco.mov"
  movie-grab-view ;; show the initial state
  repeat population-data-ticks
  [
    go 
    movie-grab-view
  ]
  movie-close
end

to final-cleanup
    update-display
    ;ask Utterances [die]
    print "All done."
end

to-report update-population-state [pers-list]
  if (pers-list = false) [
    ; currently this should not occur
    report false
  ]
  
  if empty? pers-list [
    ; This should not happen.  It means the input data is malformed.
    error "In update-population-state: get-next-state returned empty list from population data input."
  ]
  
  ; Outer loop, through all persons
  foreach pers-list [
    let pers-name (item PERSON-NAME-idx ?)
    let propn-names (item PROPOSITIONS-idx ?)
    ;let pred-names (item PREDICATES-idx ?)
    ;let obj-names  (item OBJECTS-idx ?)
    let map-relats (item MAPLINKS-idx ?)
    let propn-activations (item PROPN-ACTIVATIONS-idx ?)


    ; First inner loop, through all maplinks
    ; note: at present (popco51, 5/6/12), map-relats will include descriptions of mappings between predicates or between objects
    ; even though we are not creating any such LangElems.  I believe that the 'if not any? MapLinks ...' code below will fire
    ; on such cases, and thus call make-MapLink.  However, make-MapLink will do nothing if the LangElems don't exist.
    foreach map-relats [
      let elem-name1 (item 0 ?)
      let elem-name2 (item 1 ?)
      let activn (item 2 ?)
      
      ; note: all? applies bracket-created reporter to agentset created by applying both-ends to links.
      ask MapLinks with [ all? both-ends [elem-person-name = pers-name and (elem-name = elem-name1 or elem-name = elem-name2)] ] [
        set map-activation activn
      ]

      ; Create new map links if necessary:
      ; Check whether there is any MapLink
      ;   owned by person with pers-name and
      ;   one of whose both-ends has elem-name1 as its elem-name
      ;   and the other having elem-name2 as its elem-name.
      ; If there is no such MapLink, then create it.
      ; note: any? tests emptiness of agentset created by applying both-ends to links satisfying bracket-created reporter.
      if not any? MapLinks with [ all? both-ends [elem-person-name = pers-name and (elem-name = elem-name1 or elem-name = elem-name2)] ] [
        make-MapLink pers-name elem-name1 elem-name2 activn  ; set created-dupe-concept 
      ]
    ]
    
    ; Second inner loop, through all propositions
    ; outer parens here are required netlogo syntax voodoo when foreaching multiple lists
    (foreach propn-names propn-activations [
       ask LangElems with [elem-name = ?1 and elem-person-name = pers-name] [
         set propn-activation ?2
       ]
     ])
  ]
  
  ask LangElems [set is-heard false]

  report true
end

to-report do-conversations [convs]
  ;ask LangElems [set size current-default-size] ; set the previous iteration's recipient propositions back to normal size
  ;ask LangElems [set shape default-LangElem-shape]
  ask Utterances [die]
  
  ; An empty conversation list is OK; it just means that no conversations were reported by POPCO.
  if empty? convs [report true] ; changed from false to true 11/2011 to handle non-conversation
  
  ;let at-least-one-new-thought false
  let new-LangElems []

  foreach convs [
    let new      (item 0 ?)
    let propn     (item 1 ?)
    let speaker  (item 2 ?)
    let listener (item 3 ?)
    ;print (word new " " propn " " speaker " " listener); DEBUG
    
    ; At this point, the data read into convs was a list, and not an empty list, so we
    ; have some kind of data.  The conversation we're on ought to reference a proposition
    ; that's in the speaker.  If not, the data we're being given is bad. 
    if not any? (LangElems with [elem-person-name = speaker and elem-name = propn]) [
      error "In do-conversations: Conversation data appears to have bad format."
    ]
    
    ; if the utterance introduces a new proposition to the listener, we have to create it before we can display a link:
    if (new = 1) [
      make-LangElem listener propn ; PROPOSITION-type    
      layout-propn-canonically (one-of LangElems with [elem-person-name = listener and elem-name = propn])
                              (one-of Persons with [person-name = listener])
    ]
    
    ; find the unique speaker's proposition LangElem:
    ask LangElems with [elem-person-name = speaker and elem-name = propn] [
      ; make an utterance from the speaker's LangElem to the listener's:
      create-Utterance-to one-of LangElems with [elem-person-name = listener and elem-name = propn] ; "one-of" to convert agentset to agent: there should be only one
      ; although Utterances are directed links, the arrowheads are usually too small to see, so indicate recipient by increasing its size until next round: 
      ask one-of LangElems with [elem-person-name = listener and elem-name = propn] [
        set is-heard true
      ]
    ]
    
    ; FIXME
    if take-turns-speaking [ ; user wants to see utterances individually
      wait 1 ; without this, it goes too fast to see properly, but most of the time was spent updating the other stuff
      ; now get rid of the utterance we just created:
      ask LangElems with [elem-person-name = listener] [
        set size current-default-size
        set shape default-LangElem-shape
      ]
      ask one-of Utterances with [ all? both-ends [elem-person-name = speaker or elem-person-name = listener] ] [
        die
      ]
    ]
  ]
    
  report true
end


;to-report poss-add-LangElems [pers-name elt-names elt-type]
;  let at-least-one-new-concept false
;  
;  foreach elt-names [
;    if not any? LangElems with [elem-person-name = pers-name and elem-name = ?] [  ; shouldn't be necess to check type--Lisp makes names unique
;      set at-least-one-new-concept true
;      make-LangElem pers-name ? elt-type
;    ]
;  ]
;      
;  report at-least-one-new-concept
;end


; abbreviation: version of make-MapLink that takes 2 args, the person-name and a list containing the other 3 args
; and doesn't return a value (which isn't needed in contexts in which this is called)
to make-MapLink2 [pers-name map-relat]
;  let ignored (make-MapLink pers-name (item 0 map-relat) (item 1 map-relat) (item 2 map-relat))
  make-MapLink pers-name (item 0 map-relat) (item 1 map-relat) (item 2 map-relat)
end

; elem-namei could be a proposition, object, or predicate
; activation is initial activation value of the node mapping elem-name1 and elem-name2
; NOTE: returns true if had to create a dupe concept, false if not.
;to-report make-MapLink [ pers-name elem-name1 elem-name2 activn ]
to make-MapLink [ pers-name elem-name1 elem-name2 activn ]
  ; if both ends of the MapLink will exist, we create the MapLink.  I believe the next lines will do nothing
  ; if the LangElems don't already exist. 
  ;print (word elem-name1 " " elem-name2) ; DEBUG
  ask LangElems with [elem-person-name = pers-name and elem-name = elem-name1] [   ;  and dupe-concept = false
    create-MapLinks-with LangElems with [elem-person-name = pers-name and elem-name = elem-name2] [ ;  and dupe-concept = dupe ; dupe will be false if elem-names differ, true if same
        set hidden? true
        set map-activation activn
    ]
  ]
  
  ;report created-new-dupe
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; make-LangElem definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; See comment on dupe-concept field/var in LangElem def:
; Normally, we assign false to the dupe-concept field.
; However, when we have to display a MapLink between a concept and itself
; (i.e. mapping between same-named concepts in different analog structures),
; we need an arbitrary way of creating a new, distinguishable LangElem 
; with the same elem-person, elem-name, etc.  The normal call to make-LangElem
; sets dupe-concept to false.  make-dupe-LangElem sets dupe-concept to true.
; They both work by calling basic-make-LangElem, which does the real work.
; (A bit of a kludge.)

; to make-LangElem [pers-name elt-name elt-type]
to make-LangElem [pers-name elt-name]
  ;basic-make-LangElem pers-name elt-name elt-type 0
  basic-make-LangElem pers-name elt-name 0
end

;to make-dupe-LangElem [pers-name elt-name elt-type]
;  basic-make-LangElem pers-name elt-name elt-type true 0
;end

to make-propn-LangElem [pers-name elt-name propn-activn]
  ;basic-make-LangElem pers-name elt-name PROPOSITION-type propn-activn
  basic-make-LangElem pers-name elt-name propn-activn
end

; basic-make-LangElem
; First three args: name of person, name of this language element, and what kind of lang elem it is.
; The dupe argument only matters for predicates and objects; specifies whether this is the same concept, but from a different analog struct.
; The propn-activn argument only matters for propositions; it's the activation value from the proposition network.
; (A bit of bit of inelegance because there's no inheritance, and setting values during a create seems more efficient.)
;to basic-make-LangElem [pers-name elt-name elt-type dupe propn-activn]
to basic-make-LangElem [pers-name elt-name propn-activn]
  create-LangElems 1
  [
    set hidden? true
    
    ; Make LangElems for the same name (e.g. proposition) have same color
    ; and make LangElems for different names have different colors.
    ; This test must occur before set this LangElem's elem-name.
    ;let like-me LangElems with [elem-name = elt-name]
    ;ifelse (any? like-me)
    ;  [ set color [color] of (one-of like-me) ]
    ;  [ set color next-color ]

    set elem-person-name pers-name
    set elem-name elt-name
    ;set elem-type elt-type
    ;set dupe-concept dupe
    ;ifelse elem-type = PROPOSITION-type [
    set propn-activation propn-activn
    set-proposition-appearance self
    ;][
    ;  set size default-lang-elem-size
    ;]
    set current-default-size size
    
    ;create-OwnerLinks-with Persons with [person-name = pers-name] [set hidden? true]
  ]
end

; abbreviation for make-Person that takes as argument a list of person data lists, each of form:
  ; [person-name [proposition ...] [predicate ...] [object ...] [maplink ...]]
  ; where maplink has form: [proposition proposition activation]
to make-Person2 [pers-list]
  ;show (sentence "make-Person2: pers-list = " pers-list)  ; DEBUG
  make-Person (item PERSON-NAME-idx pers-list) (item PROPOSITIONS-idx pers-list) [] [] (item MAPLINKS-idx pers-list) (item PROPN-ACTIVATIONS-idx pers-list) ; preds, objs not in use
  ;make-Person (item PERSON-NAME-idx pers-list) (item PROPOSITIONS-idx pers-list) (item PREDICATES-idx pers-list) (item OBJECTS-idx pers-list) (item MAPLINKS-idx pers-list) (item PROPN-ACTIVATIONS-idx pers-list) 
end

; make-Person
; pers-name is a string which is the name of the person
; Each of the next three arguments is a list of strings which are names of LangElems of the three kinds.
; map-relats is a list of 3-element lists, each representing an ACME map node.
; first and second elements in a map-relat are the ACME elem-names that are mapped (e.g. propositions);
; third element is an activation value for the map node.
to make-Person [ pers-name proposition-names predicate-names object-names map-relats propn-activns]  

  ; outer parens here are required netlogo syntax voodoo when foreaching multiple lists
  (foreach proposition-names propn-activns [ 
    make-propn-LangElem pers-name ?1 ?2
  ])
  
  ; obsolete concept code
  ;if do-make-concepts [
  ;  foreach predicate-names [ make-LangElem pers-name ? PREDICATE-type]
  ;  foreach object-names [ make-LangElem pers-name ? OBJECT-type]
  ;]
  foreach map-relats [ make-MapLink2 pers-name ? ]
  
  create-Persons 1 
  [
    set hidden? true
    set color person-color
    set size person-size
    set person-name pers-name
    ;create-OwnerLinks-with LangElems with [elem-person-name = pers-name] [set hidden? true]
  ]

end

;;; PROCEDURES FOR REPORTING SUMMARY DATA ;;;

;to-report total-propn-activation [propns]
;  report (sum [propn-activation] of LangElems
;end


;;; UTILITY PROCEDURES ;;;

; return an agentset of all propositions with prefix at the beginning of their names
to-report propns-with-prefix [prefix]
  report LangElems with [name-has-prefix elem-name prefix]
;  report LangElems with [(position prefix elem-name) = 0]
end

to-report name-has-prefix [name prefix]
  report ((position prefix name) = 0)
end

to-report next-color
  let next first remaining-colors
   ifelse empty? (but-first remaining-colors)
     [set remaining-colors color-sequence] ; if we've used up all of the desirable colors, start over
     [set remaining-colors but-first remaining-colors] 
  report next
end

to reset-colors
  set remaining-colors color-sequence
end

;to draw-circle-at-xy [siz circle-color x y]  
;  move-turtle-set (get-new-circle siz circle-color) x y  ; created centered on origin. then we move it
;end

; Given a set of turtles, moves them all by x,y increments.
; Note that links internal to the turtle set will maintain their relations, but also be moved.
; Other links will be stretched.
to move-turtle-set [turtset x-increment y-increment]
  ask turtset [
    set xcor xcor + x-increment
    set ycor ycor + y-increment
  ]
end

; for convenience convert reporter get-new-circle into a regular command
;to draw-circle [circle-size circle-color]
;  let ignored (get-new-circle circle-size circle-color)
;end

; using a lot turns out to make program very slow--I think because so many turtles are created
;to-report get-new-circle [circle-size circle-color]
;  let rotation circle-size ; should maybe be calculated from given diameter?  or rewrite the rest?
;  let number-turtles-needed ceiling (2 * pi * rotation) * 10
;  let new-turtles-who-numbers []
;  
;  create-turtles number-turtles-needed [
;    set shape "circle"
;    set color circle-color
;    jump rotation
;    set new-turtles-who-numbers (fput who new-turtles-who-numbers)
;  ]
;  
;  report turtles with [member? who new-turtles-who-numbers]
;end

; Gives the effect of layout-radial around a point x,y other than the origin
; NOTE this was written before move-turtle-set, so doesn't use it.
to layout-radial-uncentered [turts lnks root-agt x y max-radius]
  layout-radial turts lnks root-agt
  ; after running layout-radial in the normal way, just move everything over to be centered on new "origin" x,y
  ask turts [
    let scale (max-radius / (min (sentence max-pxcor max-pycor (- min-pxcor) (- min-pycor))))
    let newx (xcor * scale) + x ; new coord will be relative to the fake "origin" x,y
    let newy (ycor * scale) + y
    if newx > max-pxcor [set newx (max-pxcor - layout-radial-uncentered-backoff)] ; make sure things don't fall off the edge
    if newx < min-pxcor [set newx (min-pxcor + layout-radial-uncentered-backoff)]
    if newy > max-pycor [set newy (max-pycor - layout-radial-uncentered-backoff)]
    if newy < min-pycor [set newy (min-pycor + layout-radial-uncentered-backoff)]
    setxy newx newy
  ]
end

to-report alpha-earlier-person-name [list1 list2]
  report (first list1) < (first list2)
end

to-report flatten [ lis ]
  report reverse (flatten-aux lis)
end

to-report flatten-aux [ lis ]
  let result []
    
  foreach lis 
    [
      ifelse (is-list? ?)
        [ if (not empty? ?) [ set result (sentence (flatten-aux ?) result) ] ] ; do nothing if empty
        [ set result fput ? result ]
    ]
  
  report result
end
@#$#@#$#@
GRAPHICS-WINDOW
1628
12
5892
4297
1000
1000
2.12625
1
12
1
1
1
0
0
0
1
-1000
1000
-1000
1000
1
1
1
tick
30.0

BUTTON
1049
588
1104
621
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1165
588
1220
621
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1105
588
1165
621
go once
go\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
1389
705
1559
738
show-LangElem-labels
show-LangElem-labels
1
1
-1000

SWITCH
1219
705
1389
738
show-MapLink-labels
show-MapLink-labels
1
1
-1000

SWITCH
1049
624
1219
657
show-Person-labels
show-Person-labels
0
1
-1000

SWITCH
1049
705
1219
738
take-turns-speaking
take-turns-speaking
1
1
-1000

BUTTON
1372
624
1432
657
redisplay
update-display
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
1234
655
1414
688
propositions-circle-radius
propositions-circle-radius
5
50
20
1
1
NIL
HORIZONTAL

SLIDER
1049
655
1234
688
person-circle-radius
person-circle-radius
20
1000
954
2
1
NIL
HORIZONTAL

TEXTBOX
1049
689
1199
707
Not very useful:
11
0.0
1

SWITCH
1219
624
1371
657
stop-if-no-data
stop-if-no-data
0
1
-1000

OUTPUT
460
566
1034
724
9

MONITOR
1097
246
1282
291
domain 1
(item 2 propn-cat-descs)
17
1
11

MONITOR
1387
246
1572
291
domain 2
(item 3 propn-cat-descs)
17
1
11

PLOT
1047
12
1337
267
domain 1 avgs
NIL
NIL
0.0
10.0
-1.0
1.0
true
false
"" ""
PENS

PLOT
1337
12
1627
267
domain 2 avgs
NIL
NIL
0.0
10.0
-1.0
1.0
true
false
"" ""
PENS

MONITOR
1099
536
1284
581
domain 3
(item 1 propn-cat-descs)
17
1
11

PLOT
1047
302
1337
557
domain 3 avgs
NIL
NIL
0.0
10.0
-1.0
1.0
true
false
"" ""
PENS

MONITOR
1389
536
1574
581
domain 4
(item 0 propn-cat-descs)
17
1
11

PLOT
1337
302
1627
557
domain 4 avgs
NIL
NIL
0.0
10.0
-1.0
1.0
true
false
"" ""
PENS

PLOT
452
12
744
265
domain 1 activations
NIL
NIL
0.0
10.0
-1.0
1.0
true
false
"" ""
PENS

PLOT
4
15
450
561
key
(must be wider than the longest name)
NIL
0.0
0.0
0.0
0.0
false
true
"" ""
PENS

PLOT
747
12
1036
265
domain 2 activations
NIL
NIL
0.0
10.0
-1.0
1.0
true
false
"" ""
PENS

PLOT
454
302
743
558
domain 3 activations
NIL
NIL
0.0
10.0
-1.0
1.0
true
false
"" ""
PENS

PLOT
744
302
1034
558
domain 4 activations
NIL
NIL
0.0
10.0
-1.0
1.0
true
false
"" ""
PENS

@#$#@#$#@
These notes are incomplete.

## WHAT IS IT?

This is NetLogo program tool for graphical display of output from the Common Lisp program POPCO/COHERE.  (COHERE is by Paul Thagard and colleagues.  The POPCO extension,  
and modifications of COHERE, are by Marshall Abrams.)

Despite the convenient facilities for ABM in NetLogo, I've tried to keep all of the  
basic logic of the system in the Common Lisp side, using NetLogo only for display,  
thus making the core of the program independent of NetLogo.  This means, for  
example, that you can't use Behavior Space to vary parameters at present, but  
it also means that you code up a set of runs with varying parameters in Lisp,  
and run it on some system without the overhead of NetLogo (OK--but *with* the  
overhead of Common Lisp!).

In the future, I hope to allow control of the Lisp side from the NetLogo interface,  
e.g. by using NetLogo Extensions and a JVM-based Lisp implementation such as ABCL.

## HOW IT WORKS

The agents are instances of the Person breed.  
Persons contain thoughts in a language of thought, represented by instances of the  
LangElem breed.

There are different kinds of LangElems, distinguished by the elem-type variable:  
Propositions, which represent complete thoughts, and predicates and objects,  
which represent components of propositions.  (They're all LangElems because  
they're displayed in similar ways.)

As noted below, do-make-concepts determines whether any concepts are created.  
Code which acts on concepts ends up doing nothing if there are no concepts.  
Similarly, code which acts on all LangElems will only act on propositions  
if there are no concepts.

## HOW TO USE IT

Uses file-based transfer of data from Lisp.

input data file-format:
first a list of person-states--the initial state of the population, and then:
alternating, one pair per tick, of two kinds:
  - a list of person-states (after a round of network settling)
  - a list of conversations
where a person-state is
  - [person-name list-of-mappings] i.e. this is the internal state of this person after settle-nets
    and a mapping is
       - [lang-elem-name lang-elem-name activation]
and a conversation is
  - [proposition speaker-person listener-person] i.e. the proposition is what speaker said to listener in transmit-utterance
  or better:
  - [new-flag proposition speaker-person listener-person]
    where new-flag indicates whether the proposition is new in the listener
    and (for now) is equal to either 1 (=new) or 0 (=old). (It could be a string, but why make more string-compares?)
SO THERE ARE FOUR KINDS OF RELATED OPERATIONS HERE:
1. create persons initially (no births and deaths yet, so they'll be all and only the persons forever).
2. update the map activation values
3. send a communication link between propositions of persons, adding a proposition to listener if needed.
5. update a persons network iff a new proposition has been added.


Notes on buttons, switches, etc.:

do-make-concepts determines whether turtles representing components of propositions   
get created at all.  The program runs a lot faster without them, and I'm not sure  
this aspect of the display is very useful.  (I may take it out completely in the  
future.  This would allow quit a bit of simplification of the code.)

## THINGS TO NOTICE

## THINGS TO TRY

## EXTENDING THE MODEL

## RELATED MODELS

## CREDITS AND REFERENCES

## HOW TO CITE

## COPYRIGHT NOTICE
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.2
@#$#@#$#@
set layout? false
setup repeat 175 [ go ]
repeat 35 [ layout ]
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

dashdot
0.0
-0.2 0 0.0 1.0
0.0 1 4.0 4.0 2.0 2.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

dashed
0.0
-0.2 0 0.0 1.0
0.0 1 4.0 4.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

dashedbigarrow
0.0
-0.2 0 0.0 1.0
0.0 1 4.0 4.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
Polygon -7500403 true true 0 300 150 0 300 300

dotted
0.0
-0.2 0 0.0 1.0
0.0 1 2.0 2.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
1
@#$#@#$#@
