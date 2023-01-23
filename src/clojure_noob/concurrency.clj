(ns clojure-noob.concurrency)

;; Refs

(def initial-board [[:- :k :-]
                    [:- :- :-]
                    [:- :K :-]])

(defn board-map [f board]
  (vec (map #(vec (for [s %]
                    (f s))) board)))

(defn reset-board!
  []
  (def board (board-map ref initial-board))
  (def to-move (ref [[:K [2 1]]  [:k [0 1]]]))
  (def num-moves (ref 0)))

