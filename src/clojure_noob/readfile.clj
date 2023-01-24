(ns clojure-noob.readfile
  (:require [clojure.java.io :as io]))


(defn read-file[]
  (with-open [r (io/input-stream "resources/example.txt")]
    (loop [c (.read r)]
      (if (not= c -1)
        (do 
          (print (char c))
          (recur (.read r)))))))

(defn read-file []
  (let [file (io/as-file "resources/example.txt")]
    (if (.exists file)
      (slurp file))))
