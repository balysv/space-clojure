(ns space.physics
  (use space.math)
  (:import (space.math Vec2)))

;;;; Utility

(defn all-but "Returns a sequence without the given item"
  [n v] (remove #{n} v))

;;;; Entity

(defrecord Entity
  [^{:doc "kg" :tag Double} mass
   ^{:doc "m" :tag Double} radius
   ^{:doc "m/s" :tag Vec2} velocity
   ^{:doc "m" :tag Vec2} coords
   ^{:doc "'(m)" :tag '()} path]
  ;; Compares by mass
  Comparable (compareTo [this o] (compare (:mass this) (:mass o)))
  )

(defn volume
  "Computes volume of an entity"
  [^Entity entity] (* (/ 4 3) Math/PI (Math/pow (:radius entity) 3)))

;;;; Gravity

(defn update-velocity
  "Accelerate/decellerate an entity by adding another velocity vector"
  [^Entity e ^Vec2 vec2]
  (update e :velocity (partial sum vec2)))

(defn update-position
  "Apply position of an entity and store new coordinates"
  [^Entity e]
  (-> e
      (update :coords (partial sum (:velocity e)))
      (update :path #(take 100 (conj % (:coords e))))))

(defn compute-gravity-between
  "Compute gravitational force for one entity to another"
  [^Entity e1 ^Entity e2 G]
  (let [stregth (/ (* G (:mass e1) (:mass e2)) (Math/pow (distance (:coords e1) (:coords e2)) 2))
        direction (unit (sub (:coords e2) (:coords e1)))]
    (mult direction stregth)))

(defn compute-total-gravity
  "Compute gravitational forces applied to an entity from others"
  [^Entity e others G]
  (div
    (apply merge-with + (map #(compute-gravity-between e % G) others))
    (:mass e)))

(defn apply-gravity
  "Permutation that apply gravitational forces to all entities using given
  gravitational constant. G defaults to 1.0"
  {:permutation true :experimental false}
  ([entities]
   (apply-gravity entities 1.0))
  ([entities, G]
   (map
     #(update-velocity % (compute-total-gravity % (all-but % entities) G))
     entities)))

(defn apply-movement
  "Permutation that applies positional changes by current velocities of entities"
  {:permutation true :experimental false}
  [entities]
  (map #(update-position %) entities))

;;;; Collision strategies

;; Inelastic (merge) collisions

(defn collide-inelastic
  "Inelestically merges two entities.
  Conservation of momentum: (m1 * v1 + m2 * v2) = (m1 + m2) * vf"
  [^Entity e1 ^Entity e2]
  (let [mass (+ (:mass e1) (:mass e2))
        radius (Math/pow (/ (+ (volume e1) (volume e2)) (* (/ 4 3) Math/PI)) 0.3333)
        velocity (div (sum (mult (:velocity e1) (:mass e1)) (mult (:velocity e2) (:mass e2))) mass)
        coords (div (sum (mult (:coords e1) (:mass e1)) (mult (:coords e2) (:mass e2))) mass)
        path '()]
    (Entity. mass radius velocity coords path))
  )


(defn find-inelastic-collision
  "Finds out if an entity should be merged to one of the other entities.
  If it does, the entity to be merged into is returned, else nil is returned"
  [^Entity e others]
  (loop [others others]
    (if (not-empty others)
      (let [other (first others)]
        (if (<= (distance (:coords e) (:coords other)) (max (:radius e) (:radius other)))
          other
          (recur (drop 1 others))))))
  )

(defn apply-inelastic-collisions
  "Permutation that applies fully inelastic entity collision strategy.
  Every pair of entities that touch become one by combining their attributes"
  {:permutation true :experimental true}
  [entities]
  (if (empty? entities)
    ; Short circuit input is empty
    entities
    ; else
    (loop [input entities result '()]
      (if (not-empty (rest input))
        ; Check if we can merge the first item to any of the rest
        (let [merger (find-inelastic-collision (first input) (rest input))]
          (if (nil? merger)
            ; No merger - add the item to the result and loop again
            (recur (drop 1 input) (conj result (first input)))
            ; Merger found - replace merged entities with the new one
            (recur (conj (remove #{merger} (drop 1 input)) (collide-inelastic (first input) merger)) result)))

        ; Only one entity in input - add it and return
        (conj result (first input)))
      ))
  )


;; Elastic (bounce) collisions

(defn collide-elastic
  "Applies elastic collision energy transfer from e1 to e2 and
  returns e1 with updated velocity.
  Conservation of momentum: m1 * v1 + m2 * v2 = m1 * vf1 + m2 * vf2
  Conservation of energy: (m1 * v1^2 / 2) + (m2 * v2^2 / 2) = (m1 * vf1^2 / 2) + (m2 * vf2^2 / 2)"
  [^Entity e1 ^Entity e2]
  (let [mass (+ (:mass e1) (:mass e2))
        velocity (sum (mult (:velocity e1) (/ (- (:mass e1) (:mass e2)) mass))
                      (mult (:velocity e2) (/ (* (:mass e2) 2) mass)))]
    (assoc e1 :velocity velocity))
  )


(defn find-elastic-collisions
  "Finds and applies energy elastic collision energy transfers
  between one and the rest of the entities."
  [^Entity e others]
  (loop [e e others others result '()]
    (if (not-empty others)
      (let [other (first others)]
        ; Check if entity touches the first item in others
        (if (<= (distance (:coords e) (:coords other)) (+ (:radius e) (:radius other)))
          ; Update velocities for two colliding entities and repeat without the handled entity
          (recur (collide-elastic e other) (drop 1 others) (conj result (collide-elastic other e)))
          ; Nothing collided, start again without the first item
          (recur e (drop 1 others) (conj result other))))

      ; Done iterating - combine the result into single list
      (conj result e)))
  )


(defn apply-elastic-collisions
  "Permutation that applies fully elastic entity collision strategy.
  Every pair of entities that touch bounce each other with force computed
  from convervation of energy and momentum"
  {:permutation true :experimental true}
  [entities]
  (if (empty? entities)
    ; Short circuit input is empty
    entities
    ; else
    (loop [input entities result '()]
      (if (not-empty (rest input))
        ; Check if we need to collide the first entity with the rest
        (let [updated (find-elastic-collisions (first input) (rest input))]
          ; Repeat with first entity removed
          (recur (drop 1 updated) (conj result (first updated))))

        ; Only one entity in input - add it and return
        (conj result (first input)))
      ))
  )