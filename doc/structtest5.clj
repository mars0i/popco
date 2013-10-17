;; purely functional slot rotation
;;
(def times 1000000)

(defrecord myrec [p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20])
(def recvar (->myrec 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
(defn rot-rec1 [s] (->myrec (.p20 s) (.p1 s) (.p2 s) (.p3 s) (.p4 s) (.p5 s) (.p6 s) (.p7 s) (.p8 s) (.p9 s) (.p10 s) (.p11 s) (.p12 s) (.p13 s) (.p14 s) (.p15 s) (.p16 s) (.p17 s) (.p18 s) (.p19 s)))
(defn rot-rec2 [s] (->myrec (:p20 s) (:p1 s) (:p2 s) (:p3 s) (:p4 s) (:p5 s) (:p6 s) (:p7 s) (:p8 s) (:p9 s) (:p10 s) (:p11 s) (:p12 s) (:p13 s) (:p14 s) (:p15 s) (:p16 s) (:p17 s) (:p18 s) (:p19 s)))

(defstruct mystruct :p1 :p2 :p3 :p4 :p5 :p6 :p7 :p8 :p9 :p10 :p11 :p12 :p13 :p14 :p15 :p16 :p17 :p18 :p19 :p20)
(def structvar (struct mystruct 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
(defn rot-struct [s] (struct mystruct (:p20 s) (:p1 s) (:p2 s) (:p3 s) (:p4 s) (:p5 s) (:p6 s) (:p7 s) (:p8 s) (:p9 s) (:p10 s) (:p11 s) (:p12 s) (:p13 s) (:p14 s) (:p15 s) (:p16 s) (:p17 s) (:p18 s) (:p19 s)))

(deftype mytype [p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20])
(def typevar (mytype. 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
(defn rot-type [s] (mytype. (.p20 s) (.p1 s) (.p2 s) (.p3 s) (.p4 s) (.p5 s) (.p6 s) (.p7 s) (.p8 s) (.p9 s) (.p10 s) (.p11 s) (.p12 s) (.p13 s) (.p14 s) (.p15 s) (.p16 s) (.p17 s) (.p18 s) (.p19 s)))

(defn mytest []
  (println "structs (colon accessors):")
  (time (dotimes [_ times] (rot-struct structvar)))
  (println "records with colon accessors:")
  (time (dotimes [_ times] (rot-rec2 recvar)))
  (println "records with dot accessors:")
  (time (dotimes [_ times] (rot-rec1 recvar)))
  (println "types (dot accessors):")
  (time (dotimes [_ times] (rot-type typevar)))
  )
