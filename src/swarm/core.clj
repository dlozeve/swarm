(ns swarm.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.math.numeric-tower :as math]))

(defn generate-boid [w h ]
  ;; Generate a random boid
  {:x (rand-int w)
   :y (rand-int h)
   :dx 0
   :dy 0})

(defn generate-boids [n w h]
  ;; Generate n random boids
  {:boids (vec (repeatedly n #(generate-boid w h)))})

(defn setup [n]
  (q/frame-rate 30)
  (q/color-mode :hsb)
  (generate-boids n (q/width) (q/height)))

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
