(ns drawing.perline-grid
  (:require [quil.core :as q]
            [quil.helpers.seqs :as qhs]
            [quil.middleware :as m]))

(defn setup []
  (q/smooth)
  (q/frame-rate 1)
  ;(q/color-mode :hsb)
  (q/background 240)
  {})

(defn add-noise [point]
  (let [perline-noise (* (apply q/noise point))]
    (mapv #(* perline-noise %) point)))


(defn update-state [state] (-> state))

(defn draw-state [state]
  (q/background 255)
  (let [step-size 10
        total-steps (* (q/height) (q/width) step-size)
        points (for [x (range 0 (q/width) step-size)
                     y (range 0 (q/height) step-size)]
                [x y])]


                   ;(take total-steps))]

    (println "total points: " total-steps)
    ;(println "actual points" (count lines))
    ;(println "---------point: " (take 5 lines))

    (doseq [[x1 y1] points]
      (let [pn (* 20 (q/noise x1 y1))]
        (q/fill 100 100 pn)
        (if (pos? pn)
          (q/stroke 0 0 (* 10 pn))
          (do (println "negative perlin")
            (q/stroke 0 0 (* -10 pn))))
        (q/stroke-weight 2)
        (q/ellipse x1 y1 pn pn)))))





(comment
  (q/defsketch drawing
               :title "look at this perlin noise"
               :size [500 500]
               ; setup function called only once, during sketch initialization.
               :setup setup
               ; update-state is called on each iteration before draw-state.
               :update update-state
               :draw draw-state
               :features [:keep-on-top]
               ; This sketch uses functional-mode middleware.
               ; Check quil wiki for more info about middlewares and particularly
               ; fun-mode.
               :middleware [m/fun-mode]))
