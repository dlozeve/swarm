(ns swarm.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.math.numeric-tower :as math]))

(defn generate-boid [w h]
  ;; Generate a random boid
  {:x (rand w)
   :y (rand h)
   :dx 0
   :dy 0})

(defn generate-boids [n w h]
  ;; Generate n random boids
  {:boids (vec (repeatedly n #(generate-boid w h)))})

(defn setup [n]
  (q/frame-rate 30)
  (q/color-mode :hsb)
  (generate-boids n (q/width) (q/height)))

(defn distance [b1 b2]
  ;; Get the distance between two boids
  (math/sqrt (+ (math/expt (- (:x b1) (:x b2)) 2)
                (math/expt (- (:y b1) (:y b2)) 2))))

(defn get-neighbours [k b bs]
  ;; Get the k nearest neighbours of a boid
  (rest (take (inc k) (sort-by #(distance b %) bs))))

(defn center-of-mass [boids]
  ;; Compute the center of mass of a set of boids
  {:x (/ (apply + (for [b boids] (:x b))) (count boids))
   :y (/ (apply + (for [b boids] (:y b))) (count boids))})

(defn cohesion [b neighbours]
  (let [c (center-of-mass neighbours)]
    {:x (:x b) :y (:y b)
     :dx (+ (:dx b) (/ (- (:x c) (:x b)) 10))
     :dy (+ (:dy b) (/ (- (:y c) (:y b)) 10))}))

(defn update-boid [b]
  ;; Update a boid's position
  {:x (+ (:x b) (:dx b))
   :y (+ (:y b) (:dy b))
   :dx (:dx b) :dy (:dy b)})

(defn update-state [state]
  ;; Update the positions of all boids
  {:boids (vec (for [b (:boids state)]
                  (update-boid b)))})

(defn draw-state [state]
  (q/background 255)
  (q/fill 0 0 0)
  (doseq [b (:boids state)] (q/ellipse (:x b) (:y b) 5 5)))

(q/defsketch swarm
  :title "The Swarm"
  :size [700 700]
  ; setup function called only once, during sketch initialization.
  :setup #(setup 100)
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:no-bind-output :keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
