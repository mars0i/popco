; Socrates example from H&T 1989 "Analog retrieval by constraint satisfaction"
; isomorphic analogy
(load "a/HTsOriginalSocratesForPOPCO")
(setf *silent-run?* t)
(create-nets 'citizens)
(mapc #'settle-net (get 'citizens 'members))
(print-units-alpha-sorted (get 'iso 'all-units) 2)
(quit)
