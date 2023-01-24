(ns clojure-noob.custom
   (:import [java.io File]))



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

(defn custom-flatten
  [x]
  (reduce #(conj %1 %2) [] x))

(defn custom-take 
  [n coll]
  (if (pos? n)
    (lazy-seq (my-reduce (fn [x coll]
                           (loop [result x
                                  number n
                                  remaining coll]
                             (if (= number 0)
                               result
                               (recur (conj result (first remaining)) 
                                      (dec number) (rest coll))))) [] coll))))

(defn custom-assoc
  [m k v & kvs]
  (let [result (assoc m k v)]
    (if (next kvs)
      (recur result (first kvs) (second kvs) (nnext kvs))
      result)))


(defmacro custom-when
  [test & body]
  (println body)
  (list 'if test (cons 'do body)))

(defmacro custom-cond
  [& clauses]
  (when clauses
    (list 'if (first clauses) 
          (if (second clauses)
              (second clauses))
          (cons 'cond (nnext clauses)))))

(defn rep [n]
  (cons n (lazy-seq (rep n))))

(defn custom-some
  [pred coll]
  (when-let [s (seq coll)]
    (or (pred (first s)) (recur pred (next s)))))

(defn custom-merge
  [& maps]
  (my-reduce #(conj (or %1 {}) %2) maps))

(defn merge-with-custom
  [f & maps]
  (when (some identity maps)
    (my-reduce (fn [m1 m2]
                 (my-reduce (fn [m e]
                              (let [k (key e) v (val e)]
                                (if (contains? m k)
                                  (assoc m k (f (get m k) v))
                                  (assoc m k v)))) 
                            (or m1 {}) 
                            (seq m2))) 
               maps)))

(defn custom-interleave
  ([coll]
   (seq coll))
  ([coll & colls]
   (flatten (loop [result []
                   colls (conj colls coll)]
              (if (empty? (first colls))
                result
                (recur (conj result (mapv first colls)) 
                       (map rest colls)))))))

(defn foo [x]
  (if (< x 0)
    (println "done")
    #(foo (do (println :x x) (dec x)))))

(defn custom-trampoline
  ([f]
   (let [ret (f)]
     (if (fn? ret)
       (recur ret)
       ret)))
  ([f & args]
   (custom-trampoline #(apply f args))))

(defn myfunc [a] (println "doing some work") (+ a 10))

(defn custom-memoize
  [f]
  (let [mem (atom {})]
    (fn [& args]
      (if-let [e (find @mem args)]
        (val e)
        (let [ret (apply f args)]
          (swap! mem assoc args ret)
          ret)))))

(defn custom-group-by
  [f coll]
  (my-reduce 
   (fn [ret x]
     (let [k (f x)]
       (assoc ret k (conj (get ret k []) x))))
     
   {} 
   coll))


(defn reverse-number[number]
  (loop [reverse 0
         number number]
    (if (= 0 number)
      reverse
      (recur (+ (mod number 10) (* 10 reverse)) (quot number 10)))))


(defn my-reduce
  ([f initial coll]
   (loop [result initial
          remaining coll]
     (if (empty? remaining)
       result
       (recur (f result (first remaining)) (rest remaining)))))
  ([f [head & tail]]
   (my-reduce f head tail)))

(defn- swap-numbers [ys x]
  (if-let [y (peek ys)]
    (if (< y x)
      (conj (pop ys) x y)
      (conj ys x))
    [x]))

(defn bubble-sort [xs]
  (let [ys (my-reduce swap-numbers [] xs)]
    (if (= xs ys)
      xs
      (recur ys))))

(defn fact [number]
  (if (= number 1)
    number
    (* number (fact (- number 1)))))

(defn fibo-example
  ([]
   (fibo-example 0 1))
  ([a b]
   (cons  a (lazy-seq (fibo-example b (+ a b))))))

(defn -map 
  ([f coll]
   (reduce (fn [acc v]
             (conj acc (f v)))
           []
           coll))
  ([f coll & colls]
   (let [colls (cons coll colls)]
     (-map (partial apply f) 
           (partition (count colls)
                (apply interleave colls)))))) 

(defn -map
  ([f coll]
   (reduce (fn [acc v]
             (conj acc (f v)))
           []
           coll))
  ([f coll & colls]
   (let [colls (cons coll colls)]
     (-map (partial apply f)
           (partition (count colls)
                      (apply interleave colls))))))
(defn -assoc 
  ([map key val & kvs]
   (let [ret (assoc map key val)]
     (if (next kvs)
       (recur ret (first kvs) (second kvs) (nnext kvs))
       ret))))

(defn -factorial
  [n] 
  (if (= n 1)
    n
    (* n (-factorial (dec n)))))