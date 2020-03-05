(ns drawing.circle2
  (:require [quil.core :as q]
            [quil.helpers.seqs :as qhs]
            [quil.middleware :as m]))

(def STEP-SIZE 0.5)
(def ITTERATIONS 100)
(def TOTAL-STEPS (* ITTERATIONS (/ 360 STEP-SIZE)))
(def R 200)


(defn setup []
  (q/smooth)
  (q/frame-rate 60)
  (q/color-mode :hsb)
  (q/background 240)
  {:seed [(rand-int 1000) (rand-int 1000)]})

(defn theta->xy [t]
  {:x (q/cos t)
   :y (+ (q/sin t) (* 1/360 t))
   :t t})

(defn add-noise [[xs ys] {:keys [x y t] :as point}]
  (let [itteration (int (/ t q/TWO-PI))
        perline-noise (* (/ 1 ITTERATIONS) R itteration
                         (- 0.5 (q/noise (+ 10 x) (+ 10 y) 1)))
        r-hat (+ R perline-noise)]
    (-> point
        (update :x #(* r-hat %))
        (update :y #(* r-hat %)))))


(defn rad->deg [deg]
  (/ (* deg 180) q/PI))
(defn deg->rad [deg]
  (/ (* deg q/PI) 180))

(defn update-state [state]
  (-> state
      (assoc :seed [(rand-int 1000) (rand-int 1000)])))


(def spiral-points
  (->> (qhs/steps 0 STEP-SIZE)
       (map deg->rad)
       (map theta->xy)
       (map (partial add-noise [1000 1000]))
       (take TOTAL-STEPS)
       (partition 2 1)))

(defn draw-state [{:keys [seed]}]
  (q/clear)
  ;(q/background 0 0 0)
  (q/stroke 200 50 200)
  (q/stroke-weight 1)
  (q/fill 10 100 155)
  (time
    (q/with-translation [(/ (q/width) 2)
                         (/ (q/height) 4)]
      (doseq [[{x1 :x y1 :y t1 :t}
               {x2 :x y2 :y t2 :t}] spiral-points]

          (q/line x1 y1 x2 y2)))))


(comment
  (q/defsketch drawing
               :title "You spin my circle right round"
               :size [1000 1000]
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
