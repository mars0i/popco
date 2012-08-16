;nat_selection1.lisp
;sandbox for natural selection functionality in POPCO
;Author:    Kristen Hammack
;Vers:      1.0.0 08/2012 kmh - initial coding


;; These are output to NetLogo:
(setf *propn-category-prefixes* '("S"))
(setf *propn-category-descriptions* '("Stuff"))

;; first clear everything out
(mapcar #'clear-plists (get 'folks 'members))
(clear-person-nets 'folks)
(setf *the-population* 'folks)


;; CONVENTIONS:
;; Proposition names start with a prefix indicating domain, and are mixed case after that.
;; prefix key:
;;
;;      s-  : stuff
;;
;; All-uppercase can be used to flag things temporarily.


;;Stuff info
(defvar stuff
  '(
        (something (obj-something) s-Obj-Something)
   ))

(defvar other
   `(
        (else (obj-else) s-Obj-Else)
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Put SIMILAR and SYMLINK-IF-UNITS calls here:
   
(defvar similarity 
    '(
        (similar 'something 'else (* -0.8 *ident-weight*))
    ))

; Put PRESUMED and IMPORTANT calls here:
(defvar pragmatic-relations '())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODEL PERSONS

; The template:
;(make-person 'name-of-person *the-population* propns-to-be-perceived-initially
;             `((make-struc 'target 'problem '(start (,@propns-to-subst-in-here)))
;               (make-struc 'source 'problem '(start (,@propns-to-subst-in-here)))
;               ,@semantic-specs-to-subst-in-here)
;             `(@,pragmatic-relations)
;             '()) ; put 'source or 'target in list to restrict utterances to propns in that struc

; make one model person
(make-person 'alex 'folks nil
             `((make-struc 'target 'problem '(start (,@other)) 
                                            ;'(goals (,@other-goals))
                                            )
               (make-struc 'source 'problem '(start (,@stuff)) 
                                            ;'(goals (,@stuff-goals))
                                            )
               ,@similarity)
             `(,@pragmatic-relations)
             '())
(persons-like 'alex '(bailey chris dana))




;;;**********************
;;;DEATH AND BIRTH FUNCTIONS

;;needs to remove person from the population
;;Anything else???
(defun death (person)
    (setf (get *the-population* 'members) (remove person (get *the-population* 'members))))


;;needs to take parent's input and make a new person with it, without using
;;a reference to the parent's input symbol
;;
;;2 parents or 1?
;;naming scheme??? something like parent-child1 would require
;;keeping track of how many children parent has had... also would make
;;any names past 2 generations ridiculous...
;;or should name be input to birth function?
(defun birth (parent)
    (let* ((new-person-name (concatenate 'string (symbol-name parent) "-child"))
            (new-person (make-symbol new-person-name)))
        (make-person new-person (get parent 'group) nil (get parent 'input))
        ;(create-net new-person)
        )) ;;to initialise the new person




;;;**********************


;*************************
; INITIAL SETTINGS
(setf *max-pop-ticks* 20)
(setf *do-converse* nil)             ; Whether to send utterances between persons
(setf *do-update-propn-nets* t)    ; Whether to update propn constraints from propn map units
(setf *do-report-to-netlogo* t)  ; Whether to create file for input to NetLogo 
(setf *do-report-analogy-nets-to-guess* t)
(setf *silent-run?* t)             ; If nil, use Thagard-style verbose reporting to console
;*************************

; TEST RUN
(print (get 'folks 'members))
(format t "~%Running Model Now.~%")

; To run the model
(init-pop)
;(popco)
