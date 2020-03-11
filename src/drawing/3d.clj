(ns drawing.3d
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))


(defn setup []
  (q/smooth)
  (q/frame-rate 60)
  ;(q/color-mode :hsb)
  (q/background 240)
  {:image (q/load-image "https://i.pinimg.com/originals/cb/33/0c/cb330c3fefaf3e2753c3d63d07a58714.png")})

(defn update-state [state]
  (-> state
      (update :image #(if (nil? %)
                        (q/load-image "https://i.pinimg.com/originals/cb/33/0c/cb330c3fefaf3e2753c3d63d07a58714.png")
                        %))))


(defn draw-state [{:keys [image]}]
  (q/camera 100 100 100 0 0 0 0 0 -1)
  (q/point-light 200 200 200 100 100 100)
  (q/background 255)
  (q/rotate-x (+ (/ q/PI 4) (/ (* (q/frame-count) q/PI) 128)))
  (q/rotate-y (+ (/ q/PI 4) (/ (* (q/frame-count) q/PI) 256)))
  (q/rotate-z (+ (/ q/PI 4) (/ (* (q/frame-count) q/PI) 512)))
  (if (zero? (.-width image))
    (do
      (println "derp")
      (q/text "Loading" 10 10))
    (do
      (println "here")
      ;(q/texture image)
      (q/image image 0 0)
      (q/rect 0 0 255 255))))
      ;(q/sphere-detail 5)
      ;(q/sphere 70))))



(comment
  (q/defsketch drawing
               :title "You spin my circle right round"
               :size [1000 1000]
               :renderer :opengl
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
