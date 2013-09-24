;; Some of the code from Didier Verna's "How to make Lisp go faster than C"
;; see below for notes on time trials

;; "Executive summary": Declarations and declamations can make a huge
;; difference in certain kinds of code in SBCL (and CCL), but not in ABCL 1.2.


(defun add0 (to from val)
  (let ((size (array-dimension to 0)))
    (dotimes (i size)
      (setf (aref to i) (+ (aref from i) val)))))


(defun add1 (to from val)
  (declare (type (simple-array single-float (*)) to from))
  (declare (type single-float val))
  (let ((size (array-dimension to 0)))
    (dotimes (i size)
      (setf (aref to i) (+ (aref from i) val)))))


; defvar makes sbcl happy
(defvar ra-size 0)
(defvar ra-to nil)
(defvar ra-from nil)

; but setf is needed during reloads. and make-array wants a constant size.
(setf ra-size (expt 10 8))
(setf ra-to (make-array `(,ra-size) :element-type 'single-float :initial-element 0.1))
(setf ra-from (make-array `(,ra-size) :element-type 'single-float :initial-element 0.25))

;; on OS X 10.6.8, MacBook Air 1.6 GHz Intel Core 2 Duo, 4GB 1067 MHz DDR3

;; Run one of the two declaim statements below before loading this file.

; unoptimized:
; (declaim (optimize (speed 0) (safety 3) (debug 3)))

; optimized:
; (declaim (optimize (speed 3) (safety 0) (debug 0) (space 0)))


; times for 
;       (progn (gc) (add{0,1} ra-to ra-from .3))
; which are pretty consistent across multiple trials.

;; SBCL 1.1.5 with ra-size = (expt 10 8) :
;;
;; unoptimized:
;; add0: 4.874 secs
;; add1: 2.727 secs
;;
;; optimized
;; add0: 3.816 secs
;; add1: 0.312 secs

;; !


;; In ABCL 1.2.0 with Java 1.6.0_51 Apple Inc., Java HotSpot(TM) 64-Bit Server VM,
;; neither declarations nor declamations seems to make any difference.
;; e.g. for ra-size = (expt 10 6), you get a time in the neighborhood of 1.5 secs,
;; but with no consistent direction across the two dimensions of optimization.
;; (It won't even do the size = expt 10 8 without expanding the Java heap, and I
;; though it will do expt 10 7, I got tired of waiting.  I think it's doing a lot
;; of Java gc, because running it on the midpoint between expt 10 6 and expt 10 7
;; is only about twice as slow as expt 10 6.)


;; CCL 1.7-r14925M  (DarwinX8664) with ra-size = (expt 10 8) :
;;
;; unoptimized:
;; add0: 18.3 secs
;; add1: 18.3 secs
;;
;; optimized:
;; add0: 4.5 secs
;; add1: 0.9 secs


;; ECL is very slow: About 1 min for add1 even with all optimizations.
;; Guess the compilation to C is what you'd want.
