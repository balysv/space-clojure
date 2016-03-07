(ns space.physics-test
  (:require [clojure.test :refer :all]
            [space.physics :refer :all])
  (use space.physics)
  (use space.math)
  (:import (space.math Vec2)
           (space.physics Entity)))

;; Util

(defn round
  "Round a double to the given number of significant digits"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn minus-mod
  "Subtracts the modulus of n and power function from n"
  [n mod-by]
  (- n (mod n (mod-by 1))))

(defn +-
  "Returns if one big value is approximately same as another"
  [n1 n2 mod-by]
  (= (bigint (minus-mod n1 mod-by))
     (bigint (minus-mod n2 mod-by))))

;; Tests

(def realG "Gravitational constant" (* 6.674 (Math/pow 10 -11)))

(deftest test-gravity
  (testing "--Gravity on Earth--"
    (let [Earth (Entity. (yotta 5.972) (mega 6.371) zeroVec2 zeroVec2 '())
          Apple (Entity. 0.5 0.05 zeroVec2 (Vec2. 0 (mega 6.371)) '())]

      (is (= (fmap (compute-total-gravity Apple [Earth] realG) (partial round 1))
             {:x 0.0 :y -9.8})
          "Apple should be accelerating at -9.8 m/s^2 because Earth is relative massive")

      (is (= (fmap (compute-total-gravity Earth [Apple] realG) (partial round 1))
             {:x 0.0 :y 0.0})
          "Earth should not be accelerating from the gravity of an apple")
      )
    ))

(deftest test-inelastic-collision
  (testing "--Burn Earth into the core of the Sun--"
    (let [Earth (Entity. (yotta 5.972) (mega 6.371) zeroVec2 zeroVec2 '())
          Sun (Entity. (yotta 2000000) (mega 695.7) zeroVec2 (Vec2. (mega 1) (mega 1)) '())
          Mars (Entity. (zetta 639) (mega 3.39) zeroVec2 (Vec2. (mega 1000) (mega 1000)) '())]

      (is (+- (volume Earth) (zetta 1.08) #(* 10 (exa %)))
          "Earth volume should be approx 1.08 x 10^21 m^3")

      (is (+- (volume Sun) (yotta 1410) yotta)
          "Sun volume should be approx 1.41 x 10^27 m^3")

      (is (= (find-inelastic-collision Earth [Sun]) Sun)
          "Earth should be merged into the Sun")

      (is (= (find-inelastic-collision Sun [Earth]) Earth)
          "Sun should be merged into the Earth")

      (is (= (first (apply-inelastic-collisions [Earth Sun])) (collide-inelastic Earth Sun))
          "Sun and Earth are merged into one mostly Sunish entity")

      (is (= (apply-inelastic-collisions [Earth Sun Mars]) [Mars (collide-inelastic Earth Sun)])
          "Earth gets burned down but Mars is somewhat habitable")
      )
    ))

(deftest test-elastic-collision
  (testing "--Bouncy earth bounces off bouncy Sun--"
    (let [Earth (Entity. (yotta 5.972) (mega 6.371) zeroVec2 zeroVec2 '())
          Sun (Entity. (yotta 2000000) (mega 695.7) zeroVec2 (Vec2. (mega 1) (mega 1)) '())]

      (is (not (= (find-elastic-collisions Earth (list Sun)) '(Earth Sun)))
          "Earth and Sun are not the same after colliding")
      )
    ))

(run-tests)
