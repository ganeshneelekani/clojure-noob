(ns clojure-noob.macro 
  (:require [clojure.string :as string]))

(defn criticize-code 
  [criticism code]
  `(println ~criticism (quote ~code)))

(defmacro code-critic
  [bad good]
  `(do ~(criticize-code "Cursed bacteria of Liberia, this is bad code:" bad)
       ~(criticize-code "Sweet sacred boa of Western and Eastern Samoa, this
is good code:" good)))


(defmacro code-critic
  [bad good]
  `(do ~@(map #(apply criticize-code %)
             [["Great squid of Madrid, this is bad code:" bad]
              ["Sweet gorilla of Manila, this is good code:" good]])))

(def order-details
  {:name "Ganesh Neelekani"
   :email "ganesh.com"})

(def order-details-validations
  {:name ["Please enter a name" not-empty]
   :email ["Please enter an email address "not-empty
           "Your email address doe snot look like an email address"
           #(or (empty? %) (re-seq #"@" %))]})

(defn error-messages-for
  "Return a seq of error messages"
  [to-validate message-validator-pairs]
  (map first (filter #(not ((second %) to-validate))
                     (partition 2 message-validator-pairs))))

(defn validate
  "Returns a map with a vector of errors for each key"
  [to-validate validations]
  (reduce (fn [errors validation]
            (let [[fieldname validation-check-groups] validation
                  value (get to-validate fieldname)
                  error-messages (error-messages-for value validation-check-groups)]
              (if (empty? error-messages)
                errors
                (assoc errors fieldname error-messages))))
          {}
          validations))
(validate order-details order-details-validations)


(defmacro my-print
  [expression]
  (list 'let ['result expression]
        (list 'println 'result)
        'result))

(defmacro when
  "Evaluates test. If logical true, evaluates body in an implicit do."
  {:added "1.0"}
  [test & body]
  (list 'if test (cons 'do body)))


(defmacro code-critic
  "Phrases are courtesy Hermes Conrad from Futurama"
  [bad good]
  `(do (println "Great squid of Madrid, this is bad code:"
                ~bad)
       (println "Sweet gorilla of Manila, this is good code:"
                ~good)))

(defn criticize-code
  [criticism code]
  `(println ~criticism (quote ~code)))

(defmacro code-critic
  [bad good]
  `(do ~(map #(apply criticize-code %)
             [["Great squid of Madrid, this is bad code:" bad]
              ["Sweet gorilla of Manila, this is good code:" good]])))

(defmacro without-mischief
  [& stuff-to-do]
  (let [macro-message (gensym 'message)]
    `(let [~macro-message "Oh, big deal!"]
       ~@stuff-to-do
       (println "I still need to say: " ~macro-message))))

(defmacro custom-let [expression]
  `(let [abc# ~expression]
     abc#))

(defmacro report
  [to-try]
  `(let [result# ~to-try]
     (if result#
       (println (quote ~to-try) "was successful:" result#)
       (println (quote ~to-try) "was not successful:" result#))))

(defmacro doseq-macro
  [macroname & args]
  `(do
     ~@(map (fn [arg] (list macroname arg)) args)))


(defn error-messages-for
  "Return a seq of error messages"
  [to-validate message-validator-pairs]
  (map first (filter #(not ((second %) to-validate))
                     (partition 2 message-validator-pairs))))



(defn -occurance [input]
  (let [split    (clojure.string/split input #"")
        str-atom (atom "")
        cnt      (atom 1)
        acc      (atom [])
        red (reduce (fn [_ v]
                      (add-watch str-atom :watcher
                                 (fn [_ _ prev curr]
                                   (when (= prev curr)
                                     (swap! cnt inc))

                                   (when-not (= prev curr)
                                     (swap! acc conj (str @cnt curr))
                                     (reset! cnt 1))))
                      (reset! str-atom v)
                      acc)
                    []
                    split)]
    (apply str (apply concat @red)))) 

