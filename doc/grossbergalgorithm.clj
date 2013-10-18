;;;; Notes in the form of clojure code
;;;; See grossbergalgorithm.nts for elaboration

(use 'clojure.core.matrix)
(set-current-implementation :vectorz)
;(set-current-implementation :clatrix)

(def num-nodes 300)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions

(defn rand-1+1 []
  (dec (rand 2)))

(defn nonneg [x]
  (max 0 x))

(defn nonpos [x]
  (min 0 x))

(defn dist-from-min [activn]
  (+ 1 activn))  ; think of sum as distance of neg activn from -1

(defn dist-from-max [activn]
  (- 1 activn))

(defn clip-to-extrema [x]
  (max -1 (min 1 x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data transformations
;; Steps should correspond to those listed in grossbergalgorithm.nts:
;; References:
;; "HT": Holyoak & Thagard 1989, "Analogue Retrieval by Constraint
;; Satisfaction", Cognitive Science 13, pp. 295-355. See pp. 313, 315.
;; "MR": Abrams 2013, "A Moderate Role for Cognitive Models in Agent-Based 
;; Modeling of Cultural Change", Complex Adaptive Systems Modeling 2013.

;; NEED TO TRIPLE CHECK EVERYTHING 
;; against HT, MR, network.lisp, grossbergalgorithm.nts.

;; step 0
(def activns 
  (matrix (repeatedly num-nodes rand-1+1)))
(def wts 
  (matrix (repeatedly num-nodes 
                      #(repeatedly num-nodes rand-1+1))))

;; step 1
;; ; neg activns will have no effect
(def pos-activns (emap nonneg activns)) ; max(0, a_j) in MR; o_i(t) in HT

;; step 2
(def pos-wts (emap nonneg wts)) ; w_ij > 0 in MR
(def neg-wts (emap nonpos wts)) ; w_ij < 0

;; step 3
(def pos-wtd-inputs (mmul pos-wts pos-activns)) ; p_i in MR; enet_j in HT
(def neg-wtd-inputs (mmul neg-wts pos-activns)) ; n_i in MR; inet_j in HT

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
