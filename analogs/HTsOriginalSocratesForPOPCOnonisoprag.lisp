; Socrates example from H&T 1989 "Analog retrieval by constraint satisfaction"
; simple non-isomorphic analogy
(load "a/HTsOriginalSocratesForPOPCO")
(setf *silent-run?* t)
(create-nets 'citizens) ; create-net should handle proper order of calling IMPORTANT
(mapc #'settle-net (get 'citizens 'members))
(print-units-alpha-sorted (get 'prag 'all-units) 2)
(quit)
