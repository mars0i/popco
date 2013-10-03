(defrecord mystruct [p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20])
(def structvar (->mystruct 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
(def times 10000)

(defn rot-slots [s] (->mystruct (.p20 s) (.p1 s) (.p2 s) (.p3 s) (.p4 s) (.p5 s) (.p6 s) (.p7 s) (.p8 s) (.p9 s) (.p10 s) (.p11 s) (.p12 s) (.p13 s) (.p14 s) (.p15 s) (.p16 s) (.p17 s) (.p18 s) (.p19 s)))

(defn test []
  (time (dotimes [ignored times] (rot-slots structvar))))
