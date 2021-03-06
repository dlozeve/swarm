(ns swarm.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn generate-boid []
  "Generate a random boid"
  {:x (/ (- (rand 2) 1) 10)
   :y (/ (- (rand 2) 1) 10)
   :dx 0
   :dy 0})

(defn generate-boids [n]
  "Generate n random boids"
  {:boids (vec (repeatedly n generate-boid))
   :goal  {:x 0 :y 0}})

(defn setup [n]
  (q/frame-rate 30)
  (q/color-mode :hsb)
  (generate-boids n))

(defn distance [b1 b2]
  "Get the distance between two boids"
  (q/sqrt (+ (q/sq (- (:x b1) (:x b2)))
             (q/sq (- (:y b1) (:y b2))))))

(defn get-neighbours [threshold b bs]
  "Get boids closer to a certain threshold"
  (vec (filter #(< (distance b %) threshold) bs)))

(defn center-of-mass [boids]
  "Compute the center of mass of a set of boids"
  (if (zero? (count boids))
    {:x 0 :y 0}
    {:x (float (/ (apply + (for [b boids] (:x b))) (count boids)))
     :y (float (/ (apply + (for [b boids] (:y b))) (count boids)))}))

(defn cohesion [b neighbours]
  "Cohesion rule: boids try to move towards the center of mass of
  their neighbours"
  (let [c (center-of-mass neighbours)]
    {:dx (/ (- (:x c) (:x b)) 100)
     :dy (/ (- (:y c) (:y b)) 100)}))

(defn separation [b neighbours]
  "Separation rule: boids move away from neighbours that are too close"
  (let [too-close (filter #(< (distance b %) 0.03) neighbours)
        close {:x (apply + (for [c too-close] (- (:x b) (:x c))))
               :y (apply + (for [c too-close] (- (:y b) (:y c))))}]
    {:dx (/ (:x close) 2)
     :dy (/ (:y close) 2)}))

(defn alignment [b neighbours]
  "Alignment rule: boids try to adopt the same velocity as their neighbours"
  (if (zero? (count neighbours))
    {:dx 0 :dy 0}
    {:dx (/ (apply + (for [b neighbours] (:dx b))) (* 10 (count neighbours)))
     :dy (/ (apply + (for [b neighbours] (:dy b))) (* 10 (count neighbours)))}))

(defn goal-seeking [boid goal]
  "Move towards a goal point"
  {:dx (/ (- (:x goal) (:x boid)) 100)
   :dy (/ (- (:y goal) (:y boid)) 100)})

(defn limit-velocity [dx dy]
  "Limit the maximum velocity of a boid"
  (let [m (q/sqrt (+ (* dx dx) (* dy dy)))
        max-v 0.035]
    (if (> m max-v)
      [(* max-v (/ dx m)) (* max-v (/ dy m))]
      [dx dy])))

(defn bound-position [boid]
  "Limit the position of a boid: if it moves out of the zone, get it
  to move back"
  (let [dx (cond
             (> (:x boid) 1) -0.025
             (< (:x boid) -1) 0.025
             :else 0)
        dy (cond
             (> (:y boid) 1) -0.025
             (< (:y boid) -1) 0.025
             :else 0)]
    {:dx dx :dy dy}))

(defn update-boid [b bs goal]
  "Update a boid's position"
  (let [neighbours (get-neighbours 0.5 b bs)
        dist-update [(cohesion b neighbours)
                     (separation b neighbours)
                     (alignment b (get-neighbours 0.2 b bs))
                     (goal-seeking b goal)
                     (bound-position b)
                     ]
        dx-total (+ (:dx b) (reduce #(+ %1 (:dx %2)) 0 dist-update))
        dy-total (+ (:dy b) (reduce #(+ %1 (:dy %2)) 0 dist-update))
        [dx-limited dy-limited] (limit-velocity dx-total dy-total)]
    {:x (+ (:x b) dx-limited)
     :y (+ (:y b) dy-limited)
     :dx dx-limited :dy dy-limited}))

(defn update-state [state]
  "Update the positions of all boids"
  {:boids (vec (for [b (:boids state)]
                 (update-boid b (:boids state) (:goal state))))
   :goal (:goal state)})

(defn mouse-moved [state event]
  "When the mouse is moved, set the goal to its position"
  (let [x (- (* (/ (:x event) (q/width)) 2) 1)
        y (- (* (/ (:y event) (q/height)) 2) 1)]
    (assoc-in state [:goal]
              {:x x :y y})))

(defn draw-state [state]
  (q/background-float 220)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (doseq [b (:boids state)]
    (let [disp-x (* (q/width) (:x b))
          disp-y (* (q/height) (:y b))
          dxdy-norm (q/sqrt (+ (q/sq (:dx b)) (q/sq (:dy b))))]
      (q/fill 150 255 150)
      (q/no-stroke)
      (q/ellipse disp-x disp-y 5 5))))

(q/defsketch swarm
  :title "swarm"
  :size [1000 1000]
  :setup #(setup 200)
  :update update-state
  :mouse-moved mouse-moved
  :draw draw-state
  :features [:no-bind-output :keep-on-top]
  :middleware [m/fun-mode])
