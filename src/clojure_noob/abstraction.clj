(ns clojure-noob.abstraction)

(defmulti full-moon-behavior
  (fn [were-creature]
    (:were-type were-creature)))

(defmethod full-moon-behavior :wolf
  [were-creature]
  (str (:name were-creature) " will howl and murder"))

(defmethod full-moon-behavior :simmons
  [were-creature]
  (str (:name were-creature) " will encourage people and sweat to the oldies"))

(defmethod full-moon-behavior :lion
  [were-creature]
  (str (:name were-creature) " will roar"))

(full-moon-behavior {:were-type :wolf
                     :name "Rachel from next door"})

(full-moon-behavior {:were-type :wolf
:name "Rachel from next door"})

(full-moon-behavior {:were-type :lion
                     :name "Rachel from next door"})


(defprotocol Psychodynamics
  (thoughts [x] "The data types inner most thoughts")
  (feelings-about [x] [x y] "Feelings about self or other"))

(extend-type java.lang.String
  Psychodynamics
  (thoughts [x] (str x "thinks"))
  (feelings-about
    ([x] (str x "Feeling about one parameter"))
    ([x y] (str x y "Feeling about two parameters"))))

(defprotocol WereCreature
   (full-moon-behavior [x]))


(defrecord WereWolf [name title]
  WereCreature
  (full-moon-behavior [x]
    (str name  " will howl and murder")))

(defprotocol person-details
 (get-details [_]))

(defrecord WereSimmons [name age]
   person-details
  (get-details [_]
               (str name " and " age)))







