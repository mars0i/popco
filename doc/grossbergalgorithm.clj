;;;; Notes in the form of clojure code
;;; See grossbergalgorithm.nts for elaboration
;;; Steps should correspond to those listed in grossbergalgorithm.nts.

(use 'clojure.core.matrix)
(set-current-implementation :vectorz)
;(set-current-implementation :clatrix)

(def num-nodes 300)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; References:
;; 1. network.lisp in POPCO, which is based on Thagard's ACME network.lisp.
;; 2. "HT": Holyoak & Thagard 1989, "Analogue Retrieval by Constraint
;; Satisfaction", Cognitive Science 13, pp. 295-355. See pp. 313, 315.
;; 3. "MR": Abrams 2013, "A Moderate Role for Cognitive Models in Agent-Based 
;; Modeling of Cultural Change", Complex Adaptive Systems Modeling 2013.
;; Note that the latter has errors.

;; Note the distinction in clojure.core.matrix between:
;; mul:  Multiply together corresponding elements of matrices
;;       which should have the same shape.
;; mmul: Regular matrice multiplication A * B:
;;       mul (see above) each row i of A with each column j of B,
;;       summing the result each time to produce element <i,j> of
;;       the result matrix.  (For vectors, this is inner product,
;;       with A as a row vector and B as a column vector.)
;;
;; emap maps a function over each element of a matrix to produce a new
;;      matrix.

;; Convention: Vector names are all-lower-case.  Matrices have initial cap
;; in each component of the name.

(defmacro defun 
  "Like defn, but with Lisp-style doc string placement after argvec.
  (Don't use with destructuring/multiple argument/body lists or with no
  docstring.  In those cases you're better off using defn anyway.)"
  [fn-name argvec docstring & body]
  `(defn ~fn-name ~docstring ~argvec ~@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scalar functions

(defun rand-1+1 []
  "Returns a random number in [-1, 1)."
  (dec (rand 2)))

(defun posify [x]
  "Return the non-negative number closest to x, i.e. 0 if x < 0, else x."
  (max 0 x))

(defun negify [x]
  "Return the non-positive number closest to x, i.e. 0 if x > 0, else x."
  (min 0 x))

(defun dist-from-max [activn]
  "Return the distance of activn from 1.  Note return value will be > 1
  if activn < 0."
  (- 1 activn))

(defun dist-from-min [activn]
  "Return the distance of activn from 1.  Note return value will be > 1
  if activn > 0."
  (+ 1 activn)) ; What's HT1989 p. 313 really says is (activn - -1)

(defun clip-to-extrema [x]
  "Returns -1 if x < -1, 1 if x > 1, and x otherwise."
  (max -1 (min 1 x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data transformations

;; NEED TO TRIPLE CHECK EVERYTHING 
;; against HT, MR, network.lisp, grossbergalgorithm.nts.

;; step 0: generate some data to operate on
(def activns (matrix (repeatedly num-nodes rand-1+1)))
(def Wts (matrix (repeatedly num-nodes #(repeatedly num-nodes rand-1+1))))

;; step 1
;; ; neg activns will have no effect
(def pos-activns (emap posify activns)) ; max(0, a_j) in MR; o_i(t) in HT

;; step 2
(def Pos-Wts (emap posify Wts)) ; w_ij > 0 in MR
(def Neg-Wts (emap negify Wts)) ; w_ij < 0

;; step 3
(def pos-wtd-inputs (mmul Pos-Wts pos-activns)) ; p_i in MR; enet_j in HT
(def neg-wtd-inputs (mmul Neg-Wts pos-activns)) ; n_i in MR; inet_j in HT

;; step 4
(def dists-from-max 
  (emap dist-from-max activns)) ; .99 - a_i in MR; max - a_j(t) in HT
(def dists-from-min 
  (emap dist-from-min activns)) ; incorrect in MR; a_j(t) - min in HT

;; step 5
(def decayed-activns (mul 0.9 activns)) ; incorrect in MR; a_j(t)(1-d) in HT

;; step 6
;; Almost final step
(def unclipped-new-activns  ; s_i in MR; equation p. 313 in HT
  (add decayed-activns 
       (mul pos-wtd-inputs dists-from-max)
       (mul neg-wtd-inputs dists-from-min)))

;; step 7
;; Now just pull back activns that have exceeded -1 and 1:
(def new-activns 
  (emap clip-to-extrema unclipped-new-activns)) ; a'_i in MR; p. 315 in HT
