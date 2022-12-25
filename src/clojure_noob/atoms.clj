(ns clojure-noob.atoms)

(def fred (atom {:cuddle-hunger-level 0
                 :percent-deteriorated 100}))


(swap! fred
       (fn [current-state]
         (merge-with + current-state {:cuddle-hunger-level 1})))

(swap! fred
       (fn [current-state]
         (merge-with + current-state {:cuddle-hunger-level 1
                                      :percent-deteriorated 1})))

(def sock-varieties
  #{"darned" "argyle" "wool" "horsehair" "mulleted"
    "passive-aggressive" "striped" "polka-dotted"
    "athletic" "business" "power" "invisible" "gollumed"})

(defn sock-count
  [sock-variety count]
  {:variety sock-variety
   :count count})

(defn generate-sock-gnome
  "Create an initial sock gnome state with no socks"
  [name]
  {:name name
   :socks #{}})

(def sock-gnome (ref (generate-sock-gnome "Barumpharumph")))

(def dryer (ref {:name "LG 1337"
                 :socks (set (map #(sock-count % 2) sock-varieties))}))

(defn steal-sock
  [gnome dryer]
  (dosync
   (when-let [pair (some #(if (= (:count %) 2) %) (:socks @dryer))
              ]
      (println "---pair---" pair)
     (let [updated-count (sock-count (:variety pair) 1)]
       (alter gnome update-in [:socks] conj updated-count)
       (alter dryer update-in [:socks] disj pair)
       (alter dryer update-in [:socks] conj updated-count)))))


(def counter (ref 0))
(future
  (dosync
   (alter counter inc)
   (println @counter)
   (Thread/sleep 500)
   (alter counter inc)
   (println @counter)))
(Thread/sleep 250)
(println @counter)

(def x 0)

(defn increment-number []
  (let [p (promise)]
    (let [future-var (future
                       (loop [y x]
                         (Thread/sleep 1000) 
                         (if (> y 3)
                           y
                           (recur (inc y)))))] 
      (deliver p @future-var)
      [x @p])))

;; (defn increment-number []
;;   (future 
;;     (loop [y x]
;;       (Thread/sleep 1000)
;;       (println "---y---" y)
;;       (if (> y 3)
;;         y
;;         (recur (inc y))))))

;; (let [p (promise)
;;       f (increment-number)]
;;   (deliver p @f)
;;   [@p ])


(def google "https://google.com/search?q%3D")

(def bing "https://bing.com/search?%3D")

(defn web-search [query search-engine]
  (let [result (future
                 (slurp (apply str search-engine query)))]
    @result))