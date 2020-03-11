(ns drawing.devcon
  (:require [quil.core :as q]
            [quil.helpers.seqs :as qhs]
            [quil.middleware :as m])
  (:import (java.util Queue)))

(def STEP-SIZE 1)
(def ITTERATIONS 100)
(def TOTAL-STEPS (* ITTERATIONS (/ 360 STEP-SIZE)))
(def R 200)

(defn setup []
  (q/frame-rate 60)
  (q/smooth)
  (q/background 255 255 255)
  (q/stroke 0 0 0)
  (q/fill 200 200 200)
  {:seed 1})

(defn update-state [state]
  state)

(defn bounds [points]
  (let [xs (map :x points)
        ys (map :y points)]
    [(reduce min xs)
     (reduce min ys)
     (reduce max xs)
     (reduce max ys)]))

(defn center [points]
  (let [xs (map :x points)
        ys (map :y points)
        x0 (reduce min xs)
        y0 (reduce min ys)
        x1 (reduce max xs)
        y1 (reduce max ys)]
    [(/ (- x1 x0) 2)
     (/ (- y1 y0) 2)]))

(defn radiate [{origin-x :x origin-y :y} {x :x y :y} dist]
  (let [dx (- x origin-x)
        dy (- y origin-y)
        magnitude (Math/sqrt (+ (* dx dx)
                                (* dy dy)))]
    {:x (+ x (* (/ dx magnitude) dist))
     :y (+ y (* (/ dy magnitude) dist))}))

(defn shift [point dv]
  (merge-with + point dv))

(defn scale
  ([point s]
   (-> point
       (update :x #(* s %))
       (update :y #(* s %))))
  ([point origin s]
   (shift origin (scale (shift point (scale origin -1)) s))))

;;;;;;;;;;;;;curve to points;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn line->points
  ([[x1 y1] [x2 y2] steps] (line->points x1 y1 x2 y2 steps))
  ([x1 y1 x2 y2 steps]
   (let [dx (- x2 x1)
         dy (- y2 y1)]
     (->> (qhs/range-incl 0 steps)
          (map (fn [t]
                 {:x (+ x1 (* dx (/ t steps)))
                  :y (+ y1 (* dy (/ t steps)))}))))))

(defn elipse->points
  ([[x y] width height t1 t2 steps] (elipse->points x y width height t1 t2 steps))
  ([x y width height t1 t2 steps]
   (let [step-size (/ (- t2 t1) steps)]
     (->> (qhs/range-incl t1 t2 step-size)
          (map (fn [t]
                 {:x (+ (* width (q/cos t)) x)
                  :y (+ (* height (q/sin t)) y)}))))))

(defn circle->points
  ([[x y] r t1 t2 steps] (elipse->points x y r r t1 t2 steps))
  ([x y r t1 t2 steps] (elipse->points x y r r t1 t2 steps)))

(defn points->drawn-lines [points]
  (partition 2 1 points))
;;;;;;;;;;;;;;lines;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def origin {:x 0 :y 0})
(def rx (rand-int 100))
(def ry (rand-int 100))

(defn centered-D-points [x y s]
  (let [tfn (fn [[x_ y_]] [(* s (+ x x_)) (* s (+ y y_))])
        points
        (concat
          (line->points (tfn [-3 -5])
                        (tfn [-3 5])
                        100)
          (line->points (tfn [-3 5])
                        (tfn [-1 5])
                        20)
          (elipse->points (tfn [-1 0])
                          (* s -4) (* s 5)
                          (* q/PI (double 1/2))
                          (* q/PI (double 3/2))
                          200)
          (line->points (tfn [-1 -5])
                        (tfn [-3 -5])
                        20))]
    (->> (conj points (first points))
         (map (fn [{:keys [x y]:as point}] (assoc point :nx (- 0.5 (q/noise (+ rx x) (+ ry y)))))))))


(defn centered-E-points [x y s]
  (let [tfn (fn [[x_ y_]] [(* s (+ x x_)) (* s (+ y y_))])
        points
        (concat
          (line->points (tfn [-4 -5])
                        (tfn [-4 5])
                        100)
          (line->points (tfn [-4 5])
                        (tfn [4 5])
                        100)
          (line->points (tfn [4 5])
                        (tfn [4 3])
                        20)
          (line->points (tfn [4 3])
                        (tfn [-2 3])
                        80)
          (line->points (tfn [-2 3])
                        (tfn [-2 1])
                        20)
          (line->points (tfn [-2 1])
                        (tfn [4 1])
                        80)
          (line->points (tfn [4 1])
                        (tfn [4 -1])
                        20)
          (line->points (tfn [4 -1])
                        (tfn [-2 -1])
                        80)
          (line->points (tfn [-2 -1])
                        (tfn [-2 -3])
                        20)
          (line->points (tfn [-2 -3])
                        (tfn [4 -3])
                        80)
          (line->points (tfn [4 -3])
                        (tfn [4 -5])
                        20)
          (line->points (tfn [4 -5])
                        (tfn [-4 -5])
                        100))]
    (->> (conj points (first points))
         (map (fn [{:keys [x y]:as point}] (assoc point :nx (- 0.5 (q/noise (+ rx x) (+ ry y)))))))))

(defn centered-V-points [x y s]
  (let [tfn (fn [[x_ y_]] [(* s (+ x x_)) (* s (+ y y_))])
        points
        (concat
          (line->points (tfn [-5 -5])
                        (tfn [-1 5])
                        110)
          (line->points (tfn [-1 5])
                        (tfn [1 5])
                        20)
          (line->points (tfn [1 5])
                        (tfn [5 -5])
                        110)
          (line->points (tfn [5 -5])
                        (tfn [3 -5])
                        20)
          (line->points (tfn [3 -5])
                        (tfn [0 3])
                        90)
          (line->points (tfn [0 3])
                        (tfn [-3 -5])
                        90)
          (line->points (tfn [-3 -5])
                        (tfn [-5 -5])
                        20))]
    (->> (conj points (first points))
         (map (fn [{:keys [x y]:as point}] (assoc point :nx (- 0.5 (q/noise (+ rx x) (+ ry y)))))))))

(defn draw-radiate [{nx1 :nx :as p1} {nx2 :nx :as p2} origin p-scale]
  (let [{x1 :x y1 :y t1 :t} (radiate origin (scale p1 (+ 100 (* p-scale 2))) (* 4 p-scale nx1))
        {x2 :x y2 :y t2 :t} (radiate origin (scale p2 (+ 100 (* p-scale 2))) (* 4 p-scale nx2))]
    (q/line x1 y1 x2 y2)))


(defn draw-state [{:keys [seed]}]
  (q/background 255 255 255)
  (q/fill 200 200 200)
  (q/with-translation [(+ (/ (q/width) 4))
                       (/ (q/height) 2)]
    (q/stroke-weight 1)
    (q/stroke 0 0 255)

    (doseq [[{_x1 :x _y1 :y nx1 :nx ny1 :ny :as p1}
             {_x2 :x _y2 :y nx2 :nx ny2 :ny :as p2}] (points->drawn-lines (centered-D-points 0 0 0.2))
            scale (range 0 50 2)]
      (draw-radiate p1 p2 origin scale)))
  (q/with-translation [(+ (/ (q/width) 2))
                       (+ (/ (q/height) 2))]
    (q/stroke 0 255 0)
    (doseq [[{_x1 :x _y1 :y nx1 :nx ny1 :ny :as p1}
             {_x2 :x _y2 :y nx2 :nx ny2 :ny :as p2}] (points->drawn-lines (centered-E-points 0 0 0.2))
            scale (range 0 50 2)]
      (draw-radiate p1 p2 {:x -50 :y 0} scale)))
  (q/with-translation [(+ (/ (q/width) 4/3))
                       (+ (/ (q/height) 2))]
    (q/stroke 255 0 0)
    (doseq [[{_x1 :x _y1 :y nx1 :nx ny1 :ny :as p1}
             {_x2 :x _y2 :y nx2 :nx ny2 :ny :as p2}] (points->drawn-lines (centered-V-points 0 0 0.2))
            scale (range 0 50 2)]
      (draw-radiate p1 p2 {:x 0 :y 0} scale))))


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
             :middleware [m/fun-mode])
