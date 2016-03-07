(ns space.math)

(defrecord Vec2 [^Double x ^Double y])

(def zeroVec2
  "2 dimensional vector with [0;0] coordinates"
  (Vec2. 0 0))

(defn fmap
  "Applies a function to each value of a map."
  [map f] (into {} (for [[key val] map] [key (f val)])))

(defn sum
  "Sums values of two maps."
  [^Vec2 p1 ^Vec2 p2] (merge-with + p1 p2))

(defn sub
  "Subtracts one maps values from another.
  If one map is provided, its values are negated."
  ([^Vec2 p] (fmap p #(- %)))
  ([^Vec2 p1 ^Vec2 p2] (merge-with - p1 p2)))

(defn mult
  "Multiplies values of a map by a number."
  [^Vec2 p1 n] (fmap p1 #(* % n)))

(defn div
  "Divides values of a map by a number"
  [^Vec2 p1 n] (fmap p1 #(/ % n)))

(defn magnitude
  "Calculates magninude of a vector.
  Takes the square root of the sum of squared map values.
  sqrt(x^2 + y^2 + z^2 + ...)"
  [^Vec2 v] (Math/sqrt (reduce + (map #(* % %) (vals v)))))

(defn unit
  "Calculates the unit vector."
  ([^Vec2 v]
   (let [m (magnitude v)]
     (if (== m 0)
       zeroVec2
       (fmap v #(/ % m))))))

(defn distance
  "Calculates Euclidean distance between two points"
  ([^Vec2 p1 ^Vec2 p2]
   (Math/sqrt
     (+ (Math/pow (- (:x p1) (:x p2)) 2)
        (Math/pow (- (:y p1) (:y p2)) 2)))))

(defn big-pow-10 "bigint(n * 10 ^ pow)" [n pow] (bigint (* n (Math/pow 10 pow))))
(defn kilo "10 ^ 3" [n] (big-pow-10 n 3))
(defn mega "10 ^ 6" [n] (big-pow-10 n 6))
(defn giga "10 ^ 9" [n] (big-pow-10 n 9))
(defn tera "10 ^ 12" [n] (big-pow-10 n 12))
(defn peta "10 ^ 15" [n] (big-pow-10 n 15))
(defn exa "10 ^ 18" [n] (big-pow-10 n 18))
(defn zetta "10 ^ 21" [n] (big-pow-10 n 21))
(defn yotta "10 ^ 24" [n] (big-pow-10 n 24))
(defn googol "10 ^ 100" [n] (big-pow-10 n 100))

