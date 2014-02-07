(ns conway-life.core
  (:gen-class))

(def size 3)

(def start [:alive :dead :alive
            :dead :dead :alive
            :alive :alive :dead])

(def step1 [:dead :alive :dead
            :alive :dead :alive
            :dead :alive :dead])

(defn all-neighbours [co-ord]
  (let [diffs [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]]]
    (map #(map + % co-ord)
         diffs)))

(defn legal-neighbours [co-ord]
  (filter #(every? #{0 1 2} %) (all-neighbours co-ord)))

(defn co-ord-to-index [[x y]]
  (+ x (* y size)))

(defn index-to-co-ord [i]
  [(mod i 3) (int (/ i 3))])

(defn count-live-neighbours [grid co-ord]
  (->> (legal-neighbours co-ord)
       (map co-ord-to-index)
       (map grid)
       (filter #(= :alive %))
       count))

(defn step-cell [grid index]
  (let [co-ord (index-to-co-ord index)
        live-n (count-live-neighbours grid co-ord)
        health (grid index)]
    (cond (< live-n 2) :dead
          (and (= 3 live-n) (= health :dead)) :alive
          (and (= health :alive) (or (= live-n 2) (= live-n 3))) :alive
          :else :dead)))

(defn step [grid]
  (map #(step-cell grid %) (range 0 (* size size))))
