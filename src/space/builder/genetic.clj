(ns space.builder.genetic
  (use space.physics)
  (use space.math)
  (:import (space.math Vec2)
           (space.physics Entity)
           (java.util Comparator)))

;;;; Utilities

(def r "Entity radius" (giga 2))
(def v "Minimum velocity scalar" (mega 100))
(def m "Minimum mass" (yotta 1))
(def universe-x "Universe X size in Giga metres" 1000)
(def universe-y "Universe y size in Giga metres" 1000)
(def universe-bounds "Universe bounds" (Vec2. (giga universe-x) (giga universe-y)))
(def simulation #{apply-inelastic-collisions apply-movement #(apply-gravity % 1.0)})

(defn rand-unit-vector []
  (Vec2. (- (* 2 (rand)) 1)
         (- (* 2 (rand)) 1)))

(defn rand-coords []
  (Vec2. (giga (rand-int universe-x)) (giga (rand-int universe-y))))

(defn rand-velocity []
  (mult (rand-unit-vector) (* v (rand-int 15))))

(defn rand-entity []
  (Entity. (+ m (yotta (rand-int 10000)))
           (+ r (giga (rand-int 10))),
           (rand-velocity),
           (rand-coords)
           '())
  )

;;;; Parameters


(defrecord EvolutionState [populations, iteration])

(def crossover-percent 0.5)
(def mutation-percent 0.4)
(def reproduction-percent 0.1)

(def population-count "Divisible by 10" 30)
(def population-size "Divisible by 2" 10)
(def t "Required simulation steps" 3000)

(defn populate []
  (EvolutionState.
    (for [x (range population-count) :while (< x population-count)]
      (repeatedly population-size #(rand-entity)))
    0)
  )

(defn apply-simulation
  [entities]
  (loop [e entities i 0]
    (if (< i t)
      (recur ((apply comp simulation) e) (inc i))
      ((apply comp simulation) e)
      ))
  )

(defn find-observable [entities]
  (filter #(let [diff (sub universe-bounds (:coords %))
                 x (:x diff) y (:y diff)]
            (and (> x 0) (> y 0)))
          entities)
  )

(defn fit
  [candidate]
  (let [simulated (apply-simulation candidate)
        survivor-percent (/ (count simulated) population-size)
        observable-percent (/ (count (find-observable simulated)) population-size)
        ]
    (list candidate (+ (* survivor-percent 0.1) (* observable-percent 0.9))))
  )

;; Reproduction

(defn reproduce-populations
  [sorted-populations-with-fitness, percent]
  (map #(first %) (take (* percent (count sorted-populations-with-fitness)) sorted-populations-with-fitness)))

;; Crossover

(defn crossover-candidates
  [candidate1 candidate2]
  (let [split (/ population-size 2)]
    (flatten (concat (take split (first candidate1)) (take split (first candidate2))))
    )
  )

(defn crossover-populations
  [sorted-populations-with-fitness, percent]
  (let [populations (take (* percent (count sorted-populations-with-fitness)) sorted-populations-with-fitness)
        size (count populations)]
    (if (not-empty populations)
      (repeat size (crossover-candidates (rand-nth populations) (rand-nth populations)))
      '()))
  )

;; Mutations
(defn mutate-entity
  [entity]
  (let [mutate-mass (> (rand) 0.7)
        mass (:mass entity)
        mutate-coords (> (rand) 0.4)
        coords (:coords entity)
        mutate-vel (> (rand) 0.6)
        velocity (:velocity entity)]
    (-> entity
        (assoc-in [:mass] (if mutate-mass (+ mass (yotta (- (rand-int 200000) 100000))) mass))
        (assoc-in [:coords] (if mutate-coords (rand-coords) coords))
        (assoc-in [:velocity] (if mutate-vel (sum velocity (rand-velocity)) velocity))))
  )

(defn mutate-candidate
  [candidate]
  (let [entities (first candidate)]
    (map #(mutate-entity %) entities))
  )

(defn mutate-populations
  [sorted-populations-with-fitness, percent]
  (let [populations (take (* percent (count sorted-populations-with-fitness)) sorted-populations-with-fitness)]
    (map #(mutate-candidate %) populations))
  )

;; Solver

(defn evolve
  [populations-with-fitness]
  (let
    [sorted (sort (comparator #(> (second %1) (second %2))) populations-with-fitness)
     reproduced (reproduce-populations sorted reproduction-percent)
     crossovered (crossover-populations sorted crossover-percent)
     mutated (mutate-populations sorted mutation-percent)]
    (prn "Evolving" (count sorted) "populations... ")
    (concat reproduced, crossovered, mutated))
  )


(defn gp-solve []
  (loop
    [state (populate)]
    (let [fitness (map #(fit %) (:populations state))
          solutions (filter #(> (second %) 0.9) fitness)
          max-fit (apply max (map #(second %) fitness))
          ]
      (prn "Iteration #" (:iteration state) "Max fit:" max-fit "Fit values:" (map #(second %) fitness) "Solutions:" (first solutions))
      (if (not-empty solutions)
        (first (first solutions))
        (recur (EvolutionState. (evolve fitness) (inc (:iteration state))))
        )))
  )
