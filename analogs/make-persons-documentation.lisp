; Where do persons come from?
; Example with documentation  9/2011

;; first clear everything out
(mapcar #'clear-plists (get 'wonks 'members))  ; clear-plists is in popco-utils.lisp.  Zeros out the persons' property lists.
(clear-person-nets 'wonks) ; this is in popco.lisp

;; SUMMARY:
; make-person adds the lists headed by 'make-struc to the person's input property.
; create-nets executes those lists, which creates propositions and predicates and stores them in the person.
; (It doesn't create objects--those get created by different functions in acme.lisp.)
; create-nets also calls update-nets, which creates the units and constraints for the constraint network.

; make-person is in consensus-personal.lisp
; It just stores names/pointers in the propertly list of the symbol
; for the person, and pointers back to the person in some of those other symbols.

;            person name    population name
(make-person 'wonkA         'wonks
             '()  ; this is for given-el. I don't yet use it, but probably good for indicating nodes pegged to special values like 1.0

             ; make-struc is a function in acme-personal.lisp, but it doesn't get executed until create-nets is called below.
             ; All that make-person does is store this code in the input property of the person symbol.
             ; make-struc creates analog structures, propositions, predicates (annd argument objects? maybe not),
             ; and stores pointers to them in some properties in the person (i.e. the one who is *the-person* at the time make-struc is executed.
             ;             name of struc      ; don't know what this is for--copied from other code, I think.  Ignored maybe.
             '((make-struc 'target            
                           'problem ; will be listed in struc's symbol's data-type field (does name matter?)
                           '(start  ; will be name of field in struc's symbol ('target here) containing these propositions; also fields field (does name matter?)
                              ((president-of (Saddam Iraq) 0.75 PSI)  ; these are structured propositions; will be stored in the message property of proposition
                               (invade (Iraq Kuwait) -0.5 IIK))))     ; first: pred, second: list of args, last: proposition symbol, third: initial credence
               (make-struc 'source 'problem                              ; e.g. the message property of IIK will hold (INVADE (IRAQ KUWAIT) -0.5 IIK)
                           '(start
                              ((fuhrer-of (Adolph Germany) 1.0 FHG)
                               (occupy (Germany Austria) 1.0 OGA))))))

; n-persons is in consensus.lisp
; make 10 persons just like wonkA, with names wonkA0, wonkA1, wonkA2, ...
(n-persons 'wonkA 10 0)

; create-nets is in popco.lisp
; This just calls create-net on every person in the members property of wonks, which represents a population.
; create-net then
;    (a) executes the code stored in the person's input property--i.e. the make-struc calls above
;    (b) calls update-net (from popco.lisp) on the person [see below].
(create-nets 'wonks)

; What does update-net (called by create-nets) do?
;       The main thing it does is call constraint-map on pairs of analog strucs
;       (in wonkA there are only two of them)
;       in order to create a network in which nodes are symbols representing possible mappings
;       between propositions, predicates, or objects, and
;       links are positive and negative constraints between them.  The links are represented
;       by 3-element lists [technically that's not correct] in which the linked nodes are
;       the first two elements, and the weight is the third element [actually the cddr].
;       These link lists are stored in the all-constraints property of the person.
;       The nodes are also created by constraint-map.  A list of the nodes is stored into the
;       person's all-units property by constraint-map.  It's update-net that stores the
;       links into all-constraints, though (it gets them from properties of the nodes).
