(ns clojure-noob.core
  (:import [java.util Stack Date])
  (:gen-class))
  

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn hello []
  "asd")
(def a "as")


(defn simple-arity [ & c]
  (next c))

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

(defn add-100 [number]
  (+ number 100))

(defn dec-maker [number]
  (- number 9))

(def dec9 (fn[]
            (dec-maker 10)))

(defn mapset [f coll]
  (set (for [x coll]
         (f x))))
 
(def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])

(defn matching-part
  [part a]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

;; Impliment map functionality using reduce 

(defn map-with-reduce
  ([f coll] (seq (reduce #(conj %1 (f %2)) [] coll)))
  ([f coll & colls]
   (let [colls (cons coll colls)]
     (map-with-reduce (partial apply f)
                      (partition (count colls)
                                 (apply interleave colls))))))

(defn filter-with-reduce
  [f coll] 
  (remove nil? (reduce (fn [acc c]
                         (conj acc (if (f c)
                                     c)))
                       []
                       coll)))

(def food-journal
  [{:month 1 :day 1 :human 5.3 :critter 2.3}
   {:month 1 :day 2 :human 5.1 :critter 2.0}
   {:month 2 :day 1 :human 4.9 :critter 2.1}
   {:month 2 :day 2 :human 5.0 :critter 2.5}
   {:month 3 :day 1 :human 4.2 :critter 3.3}
   {:month 3 :day 2 :human 4.0 :critter 3.8}
   {:month 4 :day 1 :human 3.7 :critter 3.9}
   {:month 4 :day 2 :human 3.7 :critter 3.6}])

(filter-with-reduce #(< (:human %) 5) food-journal)

(defn some-with-reduce
  [f coll]
  (reduce (fn [acc c]
            (when (f c)
              (reduced true)))
          []
          coll))
(def vampire-database
  {0 {:makes-blood-puns? false, :has-pulse? true :name "McFishwich"}
   1 {:makes-blood-puns? false, :has-pulse? true :name "McMackson"}
   2 {:makes-blood-puns? true, :has-pulse? false :name "Damon Salvatore"}
   3 {:makes-blood-puns? true, :has-pulse? true :name "Mickey Mouse"}})

(defn vampire-related-details
  [social-security-number]
  (Thread/sleep 1000)
  (get vampire-database social-security-number))

(defn vampire?
  [record]
  (and (:makes-blood-puns? record)
       (not (:has-pulse? record))
       record))

(defn identify-vampire
  [social-security-numbers]
  (first (filter vampire?
                 (map vampire-related-details social-security-numbers))))


;; (sum [39 4 1]) or (sum [39 4 1] 6)
(defn sum
  ([vals] (sum vals 0))
  ([vals acc]
   (comp)
   (if (empty? vals)
     acc
     (recur (rest vals) (+ (first vals) acc)))))

(defmacro infix
  "Use this macro when you pine for the notation of your childhood"
  [infixed]
  (list (second infixed) (first infixed) (last infixed)))

;; (future (Thread/sleep 4000)
;;         (println "I'll print after 4 seconds"))
;; (println "I'll print immediately")


(def gimli-headshots ["serious.jpg" "fun.jpg" "playful.jpg"])

(defn email-user
  [email-address]
  (println "Sending headshot notification to" email-address))

(defn upload-document
  "Needs to be implemented"
  [headshot]
  true)

(def yak-butter-international
  {:store "Yak Butter International"
   :price 90
   :smoothness 90})

(def butter-than-nothing
  {:store "Butter Than Nothing"
   :price 150
   :smoothness 83})

(def baby-got-yak
  {:store "Baby Got Yak"
   :price 94
   :smoothness 99})

(defn mock-api-call
  [result]
  (Thread/sleep 1000)
  result)

(defn satisfactory?
  "If the butter meets our criteria, return the butter, else return false"
  [butter]
  (and (<= (:price butter) 100)
       (>= (:smoothness butter) 97)
       butter))

(defn shuffle-speed
  [zombie]
  (future)
  (* (:cuddle-hunger-level zombie)
     (- 100 (:percent-deteriorated zombie))))

(defn shuffle-alert
  [key watched old-state new-state]
  (let [sph (shuffle-speed new-state)]
    (if (> sph 5000)
      (do
        (println "Run, you fool!")
        (println "The zombie's SPH is now " sph)
        (println "This message brought to your courtesy of " key))
      (do
        (println "All's well with " key)
        (println "Cuddle hunger: " (:cuddle-hunger-level new-state))
        (println "Percent deteriorated: " (:percent-deteriorated new-state))
        (println "SPH: " sph)))))

(defn future-example []
  (let [p (promise)]
    (future (loop [x 0]
              (if (> x 4)
                x
                (recur (inc x)))))))

(defn recur-example [a]
  (recur (inc a)))

;;(0 1 1 2 3 5 8 13)



(defn fibo-example
 ([] 
  (fibo-example 0 1))
 ([a b]
  (cons  a (lazy-seq (fibo-example b (+ a b))))))

(defn fact [x]
  (loop [n x f 1]
    (if (= n 1)
      f
      (recur (dec n) (* f n)))))

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

(defn reverse-string [args]
  (apply str (my-reduce conj () args)))

;; Reverse number
(defn reverse-number [args]
  (loop [reverse 0
         number args]
    (if (= 0 number)
      reverse
      (recur (+ (mod number 10) (* 10 reverse)) 
             (quot number 10)))))


(def integ [1, 10, 2, 5, 6, 3])

(defn highest-product-made [coll]
  (->> coll
      (sort >)
      (take 3)
      (apply *)))

(highest-product-made [1, 10, 2, 5, 6, 3])
