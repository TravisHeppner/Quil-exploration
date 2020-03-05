(ns drawing.core
  (:require [quil.core :as q]
            [quil.helpers.seqs :as qhs]
            [quil.middleware :as m]))

(defn setup []
  (q/smooth)
  (q/frame-rate 10)
  ;(q/color-mode :hsb)
  (q/background 240)
  {})

(def r-prime 20)
(def step-dist (/ 4 2 q/PI))
(defn r-hat [t]
  (+ r-prime (* 40
                (nth (qhs/perlin-noise-seq 2 1/360) (mod t 360)))))
(defn theta->xy [t]
  (let [r-hat (r-hat t)]
    [(* r-hat (q/cos t))
     (+ (* r-hat (q/sin t))
        (* step-dist t))]))

(defn rad->deg [deg]
  (/ (* deg 180) q/PI))
(defn deg->rad [deg]
  (/ (* deg q/PI) 180))

(defn update-state [state]
  (-> state))
      ;(update :color #(mod (inc %) 255))
      ;(update :step inc)))

(defn draw-state [state]
  (q/background 240)
  (let [lines (->> (range)
                   (map #(* 5 %))
                   (map deg->rad)
                   (map theta->xy)
                   (partition 2 1)
                   (take (* 10 365)))]
    (println (take-last 2 lines))
    (q/with-translation [(/ (q/width) 2)
                         (/ (q/height) 4)]
      (doseq [[p1 p2 :as line] lines]
        ;(println "---------line: " line)
        (q/fill 0 0 0)
        (q/stroke 10 10 10)
        (q/stroke-weight 1)
        (apply q/point p1)

        (q/fill 0 0 0)
        (q/stroke 0 0 0)
        (q/stroke-weight 1)
        (apply q/line line)))))


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
