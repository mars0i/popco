; FILE: IMP.lisp
; PURPOSE: basic functions for IMP, program for modelling impression formation
; Created Paul Thagard, 10-94
; This is based directly on DECO, and replaces SACS.lisp.
; 
; With modifications by Marshall Abrams 2011-2012

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTE:
;; I've added a lot of optional arguments in order to use some of this
;; code in new ways while trying not to break old functionality.
;; This has maybe gone too far.  It might be best to split apart the
;; two call trees, or rewrite in some way so as to preserve flexibility
;; in a more natural way.
;; -MA 1/2012
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; *********************************
; * FUNCTIONS FOR PARSING IMP INPUT
; *********************************

; OBSERVED notes that a person has a particular feature, which may be a stereotype
; trait, or behavior. Degree is an optional factor that indicates to what
; degree the person has the feature.
; NOTE IN POPCO THIS IS SUPERCEDED BY ROUTINES THAT CONNECT TO 'ENVIRONMENT.
(defun observed (person feature &optional (degree 1))
  (note-unit feature)
  (addfeature person feature)
  (make-symlink 'special feature (* degree *excit-weight*))
  (print-si person " is observed as: " feature " with degree " degree))

; ADDFEATURE lists the feature on the person atom.
(defun addfeature (person feature)
  (setf (get person 'features)
        (pushnew feature (get person 'features))))

; ASSOCIATE establishes the positive and negative links between
; positively and negatively associated features.
; Positive associations are multiples of *excit-weight*.
; Negative associations are multiples of *inhib-weight*
;
; In other words, associate does two things:
; 1. Makes a symlink between two units, calculating the weight to use
; 2. Gives the units initial activations and records them in *the-person*.
;
; MA ADDED optional activation values for the new featureN units.
; Uses newly added optional activation value argument to note-unit.
; If no activation values passed to associate, then activation values 
; passed to note-unit are what note-unit would have assigned anyway 
; without the optional argument--i.e. any "legacy" code will work as 
; before. -MA 9/2011
; This is exactly PT's function slightly recoded. -MA 11/2011
(defun associate (feature1
                  feature2
                  &optional (degree 1) (activ1 *init-activ*) (activ2 *init-activ*)
                            (excit-weight *excit-weight*) (inhib-weight *inhib-weight*))
  (print-si feature1 " is associated with " feature2 " to degree " degree)
  (make-symlink feature1 feature2 (calc-assoc-weight degree excit-weight inhib-weight))
  (note-unit feature1 activ1)
  (note-unit feature2 activ2))

; NO-OP: Overwrite elsewhere if desired.
(defmacro normalize-degree (degree) degree)

; CALC-ASSOC-WEIGHT
; Abstracted out from PT's ASSOCIATE -MA 11/2011
; 1/2012 added call to normalize-degree.  This can be a no-op macro
; to produce standard COHERE behavior, or something different, e.g. for POPCO.
(defun calc-assoc-weight (degree &optional (excit-weight *excit-weight*) (inhib-weight *inhib-weight*))
  (if (> degree 0)
    (* excit-weight (normalize-degree degree))
    (* inhib-weight (normalize-degree degree) -1))) ; negative association

; SET-ASSOC-WEIGHT: Set the weight of an existing association. -MA 11/2011
(defun set-assoc-weight (feature1 feature2 degree &optional (excit-weight *excit-weight*) (inhib-weight *inhib-weight*))
  (set-symlink-weight feature1 feature2 (calc-assoc-weight degree excit-weight inhib-weight)))


; IMP-RUN runs an experimental condition. The person should be the same
; as that indicated in the OBSERVED input.

(defun imp-run (person)
  (my-print "Forming impression of " person)
  ;
  ; (graph (get *the-person* 'all-units))
  (run-exp)
  (store-results person)
  )

; STORE-RESULTS notes the final activation values for all
; potential features of the person.

(defun store-results (person)
  (dolist (unit (get *the-person* 'all-units))
    (setf (get person unit)
          (activation unit)
          )
    )
  )

; COMPARE provides an easy way to compare the results across conditions.
; Each condition is associated with a different name, and the
; important features are the ones to be compared.

(defun compare (person1 person2 feature)
  (my-print person1 " is " feature " to degree " (get person1 feature))
  (my-print person2 " is " feature " to degree " (get person2 feature))
  (my-print "The difference is " (- (get person1 feature)
                                    (get person2 feature))
            )
  )
