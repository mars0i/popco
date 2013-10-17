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

(def nonpos [x]
  (min 0 x))

(defn clip-dist-from-min [activn]
  (min 1 (+ 1 activn)))  ; think of sum as distance of neg activn from -1

(defn clip-dist-from-max [activn]
  (max -1 (- 1 activn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data transformations
;; Steps should correspond to those listed in grossbergalgorithm.nts:

;; step 0
(def activns 
  (matrix (repeatedly num-nodes rand-1+1)))
(def wts 
  (matrix (repeatedly num-nodes 
                      #(repeatedly num-nodes rand-1+1))))

;; step 1
(def pos-activns (emap nonneg activns)) ; neg activns will have no effect

;; step 2
(def pos-wts (emap nonneg wts))
(def neg-wts (emap nonpos wts))

;; step 3
(def pos-wtd-inputs (mmul pos-wts pos-activns))
(def neg-wtd-inputs (mmul neg-wts pos-activns))

;; step 4
(def clipped-dists-from-min (emap clip-dist-from-min activns))
(def clipped-dists-from-max (emap clip-dist-from-max activns))

;; step 5
(def decayed-activns (mul .9 activns))

;; step 6
