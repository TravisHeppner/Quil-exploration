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
  (q/frame-rate 10)
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
     (println step-size)
     (->> (qhs/range-incl t1 t2 step-size)
          (map (fn [t]
                 {:x (+ (* width (q/cos t)) x)
                  :y (+ (* height (q/sin t)) y)}))))))

(defn circle->points
  ([[x y] r t1 t2 steps] (elipse->points x y r r t1 t2 steps))
  ([x y r t1 t2 steps] (elipse->points x y r r t1 t2 steps)))

;;;;;;;;;;;;;;lines;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def lines
  (let [points
        [(line->points 0 0
                       0 100
                       2)
         (line->points 0 100
                       20 100
                       2)
         (elipse->points 20 50
                         -40 50
                         (* q/PI (double 1/2))
                         (* q/PI (double 3/2))
                         10)

         (line->points 20 0
                       0 0
                       2)]]
    (->> (conj points (first points))
         (mapcat (partial partition 2 1)))))

(defn centered-D-points [x y s]
  (let [tfn (fn [[x_ y_]] [(* s (+ x x_)) (* s (+ y y_))])
        points
        [(line->points (tfn [-0.3 -0.5])
                       (tfn [-0.3 0.5])
                       100)
         (line->points (tfn [-0.3 0.5])
                       (tfn [-0.1 0.5])
                       10)
         (elipse->points (tfn [-0.1 0])
                         (* s -0.4) (* s 0.5)
                         (* q/PI (double 1/2))
                         (* q/PI (double 3/2))
                         200)
         (line->points (tfn [-0.1 -0.5])
                       (tfn [-0.3 -0.5])
                       10)]]
    (conj points (first points))))


(def d-lines (mapcat (partial partition 2 1) (centered-D-points 0 0 50)))
(def all-the-ds (map (fn [s] (mapcat (partial partition 2 1) (centered-D-points 0 0 s))) (range 100 300 8)))

(defn draw-state [{:keys [seed]}]
  (q/background 255 255 255)
  (q/fill 200 200 200)
  (q/with-translation [(/ (q/width) 2)
                       (/ (q/height) 2)]
    (q/stroke 0 0 0)
    (q/ellipse 0 0 2 2)

    ;(q/stroke-weight 1)
    ;(q/stroke 255 0 0)
    ;(doseq [[{x1 :x y1 :y t1 :t}
    ;         {x2 :x y2 :y t2 :t}] lines]
    ;  (q/line x1 y1 x2 y2))
    ;
    ;
    ;(q/stroke-weight 1)
    ;(q/stroke 0 255 0)
    ;(doseq [[{x1 :x y1 :y t1 :t}
    ;         {x2 :x y2 :y t2 :t}] d-lines]
    ;  (q/line x1 y1 x2 y2))


    (q/stroke-weight 1)
    (q/stroke 0 0 255)
    (let [total-ds (mod (q/frame-count) (count all-the-ds))]
      (doseq [n (range total-ds)
              [{x1 :x y1 :y t1 :t}
               {x2 :x y2 :y t2 :t}] (nth all-the-ds n)]
          (let [theta1 (q/atan (/ y1 x1))
                nx1 (* n (q/noise x1 y1) (q/cos theta1))
                ny1 (* n (q/noise x1 y1) (q/sin theta1))
                theta2 (q/atan (/ y2 x2))
                nx2 (* n (q/noise x2 y2) (q/cos theta2))
                ny2 (* n (q/noise x2 y2) (q/sin theta2))]
            (q/line (+ nx1 x1)
                    (+ ny1 y1)
                    (+ nx2 x2)
                    (+ ny2 y2)))))))


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
