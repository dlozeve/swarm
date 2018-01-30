(ns swarm.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.math.numeric-tower :as math]))

(defn generate-boid []
  ;; Generate a random boid
  {:x (/ (- (rand 2) 1) 10)
   :y (/ (- (rand 2) 1) 10)
   :dx 0
   :dy 0})

(defn generate-boids [n]
  ;; Generate n random boids
  {:boids (vec (repeatedly n generate-boid))
   :goal {:x 0 :y 0}})

(defn setup [n]
  (q/frame-rate 30)
  (q/color-mode :hsb)
  (generate-boids n))

(defn distance [b1 b2]
  ;; Get the distance between two boids
  (math/sqrt (+ (math/expt (- (:x b1) (:x b2)) 2)
                (math/expt (- (:y b1) (:y b2)) 2))))

(defn get-neighbours [k b bs]
  ;; Get the k nearest neighbours of a boid
  (vec (rest (take (inc k) (sort-by #(distance b %) bs)))))

(defn center-of-mass [boids]
  ;; Compute the center of mass of a set of boids
  (if (zero? (count boids))
    {:x 0 :y 0}
    {:x (float (/ (apply + (for [b boids] (:x b))) (count boids)))
     :y (float (/ (apply + (for [b boids] (:y b))) (count boids)))}))

(defn cohesion [b neighbours]
  ;; Cohesion rule: boids try to move towards the center of mass of
  ;; their neighbours
  (let [c (center-of-mass neighbours)]
    {:dx (/ (- (:x c) (:x b)) 100)
     :dy (/ (- (:y c) (:y b)) 100)}))

(defn separation [b neighbours]
  ;; Separation rule: boids move away from neighbours that are too
  ;; close
  (let [too-close (filter #(< (distance b %) 0.03) neighbours)
        close {:x (apply + (for [c too-close] (- (:x b) (:x c))))
               :y (apply + (for [c too-close] (- (:y b) (:y c))))}]
    {:dx (/ (:x close) 2)
     :dy (/ (:y close) 2)}))

(defn alignment [b neighbours]
  ;; Alignment rule: boids try to adopt the same velocity as their
  ;; neighbours
  (if (zero? (count neighbours))
    {:dx 0 :dy 0}
    {:dx (/ (apply + (for [b neighbours] (:dx b))) (* 8 (count neighbours)))
     :dy (/ (apply + (for [b neighbours] (:dy b))) (* 8 (count neighbours)))}))

(defn goal-seeking [boid goal]
  {:dx (/ (- (:x goal) (:x boid)) 100)
   :dy (/ (- (:y goal) (:y boid)) 100)})

(defn limit-velocity [dx dy]
  (let [m (math/sqrt (+ (* dx dx) (* dy dy)))
        max-v 0.015]
    (if (> m max-v)
      [(* max-v (/ dx m)) (* max-v (/ dy m))]
      [dx dy])))

(defn update-boid [b bs goal]
  ;; Update a boid's position
  (let [neighbours (get-neighbours 10 b bs)
        dist-update [(cohesion b neighbours)
                     (separation b neighbours)
                     (alignment b neighbours)
                     (goal-seeking b goal)]
        dx-total (reduce #(+ %1 (:dx %2)) 0 dist-update)
        dy-total (reduce #(+ %1 (:dy %2)) 0 dist-update)
        [dx-limited dy-limited] (limit-velocity dx-total dy-total)]
    {:x (+ (:x b) (+ (:dx b) dx-total))
     :y (+ (:y b) (+ (:dy b) dy-total))
     :dx dx-total :dy dy-total}))

(defn update-state [state]
  ;; Update the positions of all boids
  {:boids (vec (for [b (:boids state)]
                 (update-boid b (:boids state) (:goal state))))
   :goal (:goal state)})

(defn mouse-moved [state event]
  (let [x (- (* (/ (:x event) (q/width)) 2) 1)
        y (- (* (/ (:y event) (q/height)) 2) 1)]
    (assoc-in state [:goal]
              {:x x :y y})))

(defn draw-state [state]
  (q/background 255)
  (q/fill 0 0 0)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (doseq [b (:boids state)]
    (q/ellipse (* (q/width) (:x b)) (* (q/height) (:y b)) 5 5)))

(q/defsketch swarm
  :title "The Swarm"
  :size [700 700]
                                        ; setup function called only once, during sketch initialization.
  :setup #(setup 100)
                                        ; update-state is called on each iteration before draw-state.
  :update update-state
  :mouse-moved mouse-moved
  :draw draw-state
  :features [:no-bind-output :keep-on-top]
                                        ; This sketch uses functional-mode middleware.
                                        ; Check quil wiki for more info about middlewares and particularly
                                        ; fun-mode.
  :middleware [m/fun-mode])
