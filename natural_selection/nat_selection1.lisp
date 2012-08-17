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

;;Removes person from *the-population* and uninterns the symbol for the person
;;I think this still introduces a memory leak, but maybe sbcl is smart enough to kill the
;;symbol after a load?
(defun death (person &optional (population *the-population*))
  (cond ((eq (get person 'group) population) 
         (setf (get population 'members) (remove person (get population 'members)))
         (unintern person)
         (get population 'members))
        (t (error "~%~a is not a member of ~a~%" person population))))


;;Takes the parent's 'input and makes a new person from it, initializing
;;the person into *the-population*
;;
;;Right now naming scheme is parent-child, which doesn't allow for multiple children.
;;Also this makes multiple generations somewhat unwieldy.
;;
;;Could make the name be an input into the function, but geneologies wouldn't be 
;;as regulated... which I guess isn't really a problem if we trust the user...
(defun birth (parent)
  (let ((new-person (gentemp "P")))
    (make-person new-person (get parent 'group) nil (get parent 'input))
    (setf (get new-person 'parent) parent)
    (create-net new-person)) ;;to initialise the new person
  new-person)




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
