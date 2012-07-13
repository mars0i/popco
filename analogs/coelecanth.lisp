; coelecanth.lisp
; popco version of coelecanth analogy copied from pp. 30ff (in Hot Thought)
; of Thagard and Shelley's "Emotional Analogies and Analogical Inference"

(mapcar #'clear-plists (get 'ichthyologists 'members))
(clear-person-nets 'ichthyologists )

(defvar fishers 'ichthyologists)

(format t "Making ichthyologists...~%")


; The Thagard and Shelley paper suggests that you can have multiple sources, but
; I don't see how that works--with my use of the code, at least.  What is the
; map node CAL_HAVE=HAVE ?  That's a mapping between the predicate HAVE in
; target to predicate HAVE in a source, but which one?  Isn't it mashing
; stuff together irresponsibly?  So for now I'm commenting out one of
; T&S's source analogs.

(make-person 'cal 'ichthyologists '()
             '((make-struc 'target 'problem
                           '(start
                              ((have (coelecanth rod-pigment-3) have-3)
                               (absorb (rod-pigment-3 473nm-light) absorb-3)
                               (penetrate (473nm-light deep-ocean-water) penetrate-3)
                               (see-in (coelcanth deep-ocean-water) see-in-3)
                               (ENABLE (have-3 see-in-3) enable-3)            ; *higher-order relation*
                               (BECAUSE (absorb-3 penetrate-3) because-3))))  ; *higher-order relation*
               (make-struc 'source1 'problem
                           '(start
                              ((have (centroscymnus rod-pigment-1) have-1)
                               (absorb (rod-pigment-1 472nm-light) absorb-1)
                               (penetrate (472nm-light deep-ocean-water) penetrate-1)
                               (see-in (centroscymnus deep-ocean-water) see-in-1)
                               (inhabit (centroscymnus deep-ocean-water) inhabit-1) ; not in target
                               (ENABLE (have-1 see-in-1) enable-1)  ; *higher-order relation*
                               (BECAUSE (absorb-1 penetrate-1) because-1)  ; *higher-order relation*
                               (ADAPT (see-in-1 inhabit-1) adapt-1)))) ; not in target, *higher-order relation*
;               (make-struc 'source2 'problem
;                           '(start
;                              ((have (ruvettus rod-pigment-2) have-2)
;                               (absorb (rod-pigment-2 474nm-light) absorb-2)
;                               (penetrate (474nm-light deep-ocean-water) penetrate-2)
;                               (see-in (ruvettus deep-ocean-water) see-in-2)
;                               (inhabit (ruvettus deep-ocean-water) inhabit-2) ; not in target
;                               (enable (have-2 see-in-2) enable-2)
;                               (because (absorb-2 penetrate-2) because-2)
;                               (adapt (see-in-2 inhabit-2) adapt-2)))) ; not in target
               ))

(create-nets 'ichthyologists) ; this step is required
(format t "Ichthyologists (fishers) made...~%")
(my-print (get 'ichthyologists 'members))
