(ns drawing.circl
  (:require [quil.core :as q]
            [quil.helpers.seqs :as qhs]
            [quil.middleware :as m]))

(defn setup []
  (q/smooth)
  (q/frame-rate 1)
  ;(q/color-mode :hsb)
  (q/background 240)
  {:seed [(rand-int 1000) (rand-int 1000)]})

(def r-prime 100)
(defn theta->xy [t]
    [(q/cos t)
     (+ (q/sin t) (* 1/360 t))])

(defn translate [shift point]
  (mapv + point shift))
(defn add-noise [perline-seed [point t]]
  (let [perline-noise (* 3
                         (/ t q/TWO-PI)
                         (apply q/noise
                                (translate (conj perline-seed (/ t q/TWO-PI)) point)))
        r-hat (/ (+ r-prime perline-noise) 2)]
    (mapv #(* r-hat %) point)))


(defn rad->deg [deg]
  (/ (* deg 180) q/PI))
(defn deg->rad [deg]
  (/ (* deg q/PI) 180))

(defn update-state [state]
  (-> state
      (assoc :seed [(rand-int 1000) (rand-int 1000)])))
;(update :color #(mod (inc %) 255))
;(update :step inc)))

(defn draw-state [{:keys [seed]}]
  (q/background 240)
  (let [step-size 1
        total-steps (* 100 (/ 360 step-size))
        lines (->> (qhs/steps 0 step-size)
                   (map deg->rad)
                   (map (juxt theta->xy identity))
                   (map (juxt (partial add-noise seed) second))
                   (take total-steps)
                   (partition 2 1))]
    (q/with-translation [(/ (q/width) 2)
                         (/ (q/height) 4)]
      (doseq [[p1 p2 :as line] lines]
        (let [color (/ (rad->deg (last p1)) total-steps)
              r (- 255 (* 135 color))
              g (+ 0   (* 155 color))
              b (+ 100 (* 200 color))
              a (+ 10 (* 40 color))
              w (+ 1 (* 2 color))]
          (q/stroke r g b a)
          (q/stroke-weight w)
          (apply q/line (map drop-last line)))))))


(comment
  (q/defsketch drawing
               :title "You spin my circle right round"
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
