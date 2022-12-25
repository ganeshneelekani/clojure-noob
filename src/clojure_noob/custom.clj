(ns clojure-noob.custom
  (:require [clojure.string :as str]))



;; ((custom-comp inc dec) 9)

(defn my-assoc [map key val & kvs]
  (let [ret (assoc map key val)]
    (if kvs
      (if (next kvs)
        (recur ret (first kvs) (second kvs) (nnext kvs)))
      ret)))

(defn my-reduce
  ([f initial coll]
   (loop [result initial
          remaining coll]
     (if (empty? remaining)
       result
       (recur (f result (first remaining)) (rest remaining)))))
  ([f [head & tail]]
   (my-reduce f head tail)))

(defn custom-comp1 [& fs]
  (my-reduce (fn [result f]
               (println "----f-----"f)
               (fn [& args] 
                 (println "----f1-----" f)
                 (apply f args)))
             identity
             fs))


(defn my-assoc-in
  [mp [ky & kysq] val]
  (if kysq
    (assoc mp ky (my-assoc-in {} kysq val))
    (assoc mp ky val)))

(defn my-update-in
  [m [k & ks] f]
  (if ks
    (assoc m k (f (my-update-in (get m k) ks f)))
    (assoc m k (f (get m k)))))

(defn custom-map
  ([f coll] (seq (reduce #(conj %1 (f %2)) [] coll)))
  ([f coll & colls]
   (let [colls (cons coll colls)]
         (custom-map (partial apply f)
                          (partition (count colls)
                                     (apply interleave colls))))))

(defn my-reduce-fn
  ([f initial coll]
   (loop [result initial
          remaining coll]
     (if (empty? remaining)
       result
       (recur (f result (first remaining)) (rest remaining)))))
  ([f [head & tail]]
   (my-reduce-fn f head tail)))

(defn update-map-entries [m e]
  (reduce #(update-in %1 [(first %2)] (fn [_] (last %2))) m e))

(defn reduce-factorial [x]
  (reduce
   (fn [a b] (conj a (+ (last a) (last (butlast a)))))
   [0 1]
   (range x)))

(defn custom-frequency
  [coll]
  (reduce (fn [count x]
            (assoc count x (inc (or (get count x) 0))))
          {}
          coll))

(defn custom-repeat
  ([n x]
   (loop [count  1
          result []]
     (if (> count n)
       result
       (recur (inc count) (conj result x))))))

(defn custom-comp
  ([]identity)
  ([f] f)
  ([f g]
   (fn 
     ([] (f (g)))
     ([x] (f (g x)))
     ([x y] (f (g x y)))
     ([x y z] (f (g x y z)))
     ([x y z & args] (f (apply g x y z args)))))
  ([f g & fs]
   (my-reduce custom-comp (list* f g fs))))

(defn custom-partial
  ([f]
   f)
  ([f & x]
   (fn 
     ([& args] (apply f (concat x args))))))

(defn custom-juxt [& fs]
  (let [fs (concat fs)]
    (fn 
      ([& args]
       (reduce #(conj %1 (apply %2 args)) [] fs)))))
 
;; fnil look for core logic

(defn custom-identity 
  [x]
  x)

(defn custom-constantly
  [x]
  (fn [& args]
    x))

(defn custom-concat-reduce [coll colls]
  (reduce (fn [initial colls]
            (reduce #(conj %1 %2) initial colls))
          coll
          colls))

(defn custom-concat
  ([] (lazy-seq nil))
  ([x] (lazy-seq
        (if (seq x)
          x)))
  ([x y] (lazy-seq (reduce #(conj %1 %2) x y)))
  ([x y & args]
   (custom-concat-reduce x (cons y args))))


(defn take-fn
  "Returns a lazy sequence of the first n items in coll, or all items if
  there are fewer than n.  Returns a stateful transducer when
  no collection is provided."
  {:added "1.0"
   :static true}
  ([n]
   (println "---n--" n)
   (fn [rf]
     (println "---rf--"rf)
     (let [nv (volatile! n)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [n @nv
                nn (vswap! nv dec)
                result (if (pos? n)
                         (rf result input)
                         result)]
            (if (not (pos? nn))
              (ensure-reduced result)
              result)))))))
  ([n coll]
   (lazy-seq
    (when (pos? n)
      (when-let [s (seq coll)]
        (cons (first s) (take (dec n) (rest s))))))))


(defn custom-flatten
  [x]
  (reduce #(conj %1 %2) [] x))


