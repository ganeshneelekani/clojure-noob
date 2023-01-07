(ns clojure-noob.vampire
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))


(def filename "suspects.csv")

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity
                  :glitter-index str->int})

(defn convert
  [vamp-keys value]
  ((get conversions vamp-keys) value))

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (apply assoc {} (interleave vamp-keys unmapped-row))))
       rows))

(defn glitter-filter
  [minimum-glitter records]
  (->> (filter #(>= (:glitter-index %) minimum-glitter) records) 
       #_(map #(get % :name))))

(def result (glitter-filter 3 (mapify (parse (slurp filename)))))

(defn validate-vampire-details? [result input]
  (remove nil? (map (fn [{:keys [glitter-index name]}] 
               (if (and (= name (:name input))
                        (= glitter-index (:glitter-index input)))
                 input))
             result)))

(def new-record {:name "Edward Cullen" :glitter-index 12})

(defn append-record []
  (if (seq (validate-vampire-details? result new-record))
    result
    (conj result new-record)))

(defn concat-str [record]
  (conj [] (clojure.string/join "," [(:name record) (:glitter-index record)])))

(defn write-to-csv []
  (with-open [writer (io/writer "1.csv")]
    (csv/write-csv writer  (mapv concat-str (append-record)))))

(defn two-comp
  [f g]
  (fn [& args]
    (f (apply g args))))

(def two-comp-var (two-comp inc +))

(def character
  {:name "Smooches McCutes"
   :attributes {:intelligence 10
                :strength 4
                :dexterity 5}})

(def c-int (comp :intelligence :attributes))

(defn custom-comp
  ([] identity)
  ([f] f)
  ([f g]
   (fn 
     ([] (f (g)))
     ([x] (f (g x)))
      ([x y] (f (g x y))))))


(defn custom-reduce 
  ([f initial colls]
   (loop [result initial
          remaining colls]
     (if (empty? remaining)
       result
       (recur (f result (first remaining)) (rest remaining)))))
  ([f [head & tail]]
   (custom-reduce f head tail)))


(defn custom-map
  ([f coll]
    (seq (reduce #(conj %1 (f %2)) [] coll)))
  ([f coll & colls]
   (let [colls (cons coll colls)]
     (custom-map (partial apply f)
                 (partition (count colls)
                            (apply interleave colls))))))

(defn custom-every?
  [pred coll]
  (cond
    (nil? (seq coll)) true
    (pred (first coll)) (recur pred (next coll))
    :else false))

