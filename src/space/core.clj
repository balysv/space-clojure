(ns space.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (use space.physics)
  (use space.math)
  (use space.builder.genetic)
  (:import (java.util Date)
           (space.physics Entity)
           (space.math Vec2)))

;; New entity creation utils

(defn time-millis "Current time in millis" [] (.getTime (Date.)))
(defn to-vec2 "Creates a Vec2 from a mouse event" [event] (map->Vec2 (select-keys event [:x :y])))

(def start-mass "Initial entity massy" (giga 1))
(def start-radius "Initial entity radius" (giga 1))

(defn get-mass "Entity mass correlated with duration of its existance"
  [duration]
  (yotta (* duration (Math/log duration))))

(defn get-radius "Entity radius correleated with duration of its existance"
  [duration]
  (giga (/ duration 100)))

(defn get-coords "Entity coordinates from position in window"
  [event]
  (fmap (to-vec2 event) #(* (giga 1) %)))

(defn get-velocity "Entity Velocity from two coordinates in window"
  [last-coords event]
  (fmap (merge-with - (to-vec2 event) (to-vec2 last-coords))
        #(* (mega 5) %)))

;; Colors

(def memo-inside-color
  "Inside color from mass of an entity"
  (memoize
    (defn get-inside-color [mass]
      (let [b (- 255 (min 255 (/ mass (yotta 100))))
            g (- 255 (max 0 (min 255 (/ (- mass (yotta 100)) (yotta 500)))))
            r (- 255 (max 0 (min 255 (/ (- mass (yotta 500)) (yotta 1500)))))]
        (list r g b)))))

(def memo-stroke-color
  "Stroke color from mass of an entity"
  (memoize
    (defn get-stroke-color [mass] (map #(+ 50 %) (memo-inside-color mass)))))


;; Update loop

(defrecord State [entities, permutations, pending])

(def G 1.0)
(def permutations #{apply-inelastic-collisions apply-movement #(apply-gravity % G)})

(defn setup []
  (q/smooth)
  (q/frame-rate 60)
  (q/background 0)
  (State. (gp-solve)
          permutations nil))

(defn update-state [state]
  (State.

    ;; Apply permutations to entities
    ((apply comp (:permutations state)) (:entities state))

    ;; Update permutations
    permutations

    ;; Apply changes to entity creation
    (:pending
      (if (some? (:entity (:pending state)))
        (let [duration (- (time-millis) (:time (:pending state)))]
          (-> state
              (assoc-in [:pending :entity :radius] (get-radius duration))
              (assoc-in [:pending :entity :mass] (get-mass duration)))
          )))
    )
  )

(defn mouse-pressed [state event]
  (-> state
      (assoc-in [:pending :start] (to-vec2 event))
      (assoc-in [:pending :end] (to-vec2 event))
      (assoc-in [:pending :time] (time-millis))
      (assoc-in [:pending :entity] (Entity. start-mass start-radius zeroVec2 (get-coords event) '())))
  )

(defn mouse-dragged [state event]
  (-> state
      (assoc-in [:pending :entity :velocity] (get-velocity (:start (:pending state)) event))
      (assoc-in [:pending :end] (to-vec2 event)))
  )

(defn mouse-released [state event]
  (-> state
      (assoc :entities (conj (:entities state) (:entity (:pending state))))
      (dissoc :pending))
  )

(defn draw-state [state]
  (q/background 0)

  ;; Draw entities
  (doseq
    [e (:entities state)]
    (q/stroke-weight 2)
    (apply q/fill (memo-inside-color (:mass e)))
    (apply q/stroke (memo-stroke-color (:mass e)))
    (apply q/ellipse (map #(/ % (giga 1)) [(:x (:coords e)) (:y (:coords e)) (* 2 (:radius e)) (* 2 (:radius e))])))

  (if (some? (:pending state))
    (let [e (get-in state [:pending :entity])]

      ;; Draw pending entity
      (q/stroke-weight 2)
      (apply q/fill (memo-inside-color (:mass e)))
      (apply q/stroke (memo-stroke-color (:mass e)))
      (apply q/ellipse (map #(/ % (giga 1)) [(:x (:coords e)) (:y (:coords e)) (* 2 (:radius e)) (* 2 (:radius e))]))

      ;; Draw velocity line
      (q/stroke-weight 1)
      (apply q/stroke (memo-inside-color (:mass e)))
      (apply q/line (concat (vals (get-in state [:pending :start])) (vals (get-in state [:pending :end])))))
    )
  )

;; Config

(defn show-frame-rate [options]
  (let [
        draw (:draw options (fn []))
        updated-draw (fn []
                       (draw)
                       (q/fill 255)
                       (q/text-num (q/current-frame-rate) 10 10))]
    (assoc options :draw updated-draw)))

(q/defsketch space
             :size [1000 1000]
             :setup setup
             :draw draw-state
             :update update-state
             :mouse-pressed mouse-pressed
             :mouse-released mouse-released
             :mouse-dragged mouse-dragged
             :middleware [show-frame-rate m/fun-mode])

(defn -main [& args] (println "Creating Space..."))

