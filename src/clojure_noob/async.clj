(ns clojure-noob.async
  (:require [clojure.core.async 
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]]))

(def echo-chan (chan))

(go (println (<! echo-chan)))

(defn hot-dog-machine
  []
  (let [in (chan)
        out (chan)]
    (go (<! in)
        (>! out "hot dog"))
    [in out]))

(let [[in out] (hot-dog-machine)]
  (>!! in "pocket lint")
  (<!! out))

(defn hot-dog-machine-v2
  [hot-dog-count]
  (let [in (chan)
        out (chan)]
    (go (loop [hc hot-dog-count]
          (if (> hc 0)
            (let [input (<! in)]
             (if (= 3 input)
               (do (>! out "hot dog")
                   (recur (dec hc)))
               (do (>! out "wilted lettuce")
                   (recur hc))) )
            (do (close! in)
                (close! out)) )))
    [in out]))

(defn add [a b]
  (+ a b))

(def p (partial + ))

(p 5 6 7)
