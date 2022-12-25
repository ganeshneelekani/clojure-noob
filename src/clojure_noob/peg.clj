(ns clojure-noob.peg
  (:require [clojure.set :as set])
  (:gen-class))

(defn tri*
  "Generates lazy sequence of triangular numbers"
  ([] (tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
     (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri (tri*))

(defn row-tri
  [n]
  (last (take n tri)))

(defn row-num
  [pos]
  (inc (count (take-while #(> pos %) tri))))

(defn connect
  "Form a mutual connection between two positions"
  [board max-pos pos neighbor destination]
  (if (<= destination max-pos)
    (reduce (fn [new-board [p1 p2]]
              (assoc-in new-board [p1 :connections p2] neighbor))
            board
            [[pos destination] [destination pos]])
    board))

(defn triangular?
  "Is the number triangular? e.g. 1, 3, 6, 10, 15, etc"
  [n]
  (= n (last (take-while #(>= n %) tri))))

(defn connect-right
  [board max-pos pos]
  (let [neighbor (inc pos)
        destination (inc neighbor)]
    (if-not (or (triangular? neighbor) (triangular? pos))
      (connect board max-pos pos neighbor destination)
      board)))

(defn connect-down-left
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ row pos)
        destination (+ 1 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn connect-down-right
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ 1 row pos)
        destination (+ 2 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn add-pos
  "Pegs the position and performs connections"
  [board max-pos pos]
  (let [pegged-board (assoc-in board [pos :pegged] true)]
    (reduce (fn [new-board connection-creation-fn]
              (connection-creation-fn new-board max-pos pos))
            pegged-board
            [connect-right connect-down-left connect-down-right])))

;; (defn clean
;;   [text]
;;   (reduce (fn [string string-fn] (string-fn string))
;;           text
;;           [s/trim #(s/replace % #"lol" "LOL")]))

(defn new-board
  "Creates a new board with the given number of rows"
  [rows]
  (let [initial-board {:rows rows}
        max-pos (row-tri rows)]
    (reduce (fn [board pos] (add-pos board max-pos pos))
            initial-board
            (range 1 (inc max-pos)))))

(defn pegged?
  "Does the position have a peg in it?"
  [board pos]
  (get-in board [pos :pegged]))

(defn remove-peg
  "Take the peg at given position out of the board"
  [board pos]
  (assoc-in board [pos :pegged] false))

(defn place-peg
  "Put a peg in the board at given position"
  [board pos]
  (assoc-in board [pos :pegged] true))

(defn move-peg
  "Take peg out of p1 and place it in p2"
  [board p1 p2]
  (place-peg (remove-peg board p1) p2))

(defn valid-moves
  "Return a map of all valid moves for pos, where the key is the
destination and the value is the jumped position"
  [board pos]
  (into {}
        (filter (fn [[destination jumped]]
                  (and (not (pegged? board destination))
                       (pegged? board jumped)))
                (get-in board [pos :connections]))))

(def my-board (assoc-in (new-board 5) [4 :pegged] false))

(defn valid-move?
  "Return jumped position if the move from p1 to p2 is valid, nil
otherwise"
  [board p1 p2]
  (get (valid-moves board p1) p2))

(defn make-move
  "Move peg from p1 to p2, removing jumped peg"
  [board p1 p2]
  (if-let [jumped (valid-move? board p1 p2)]
    (move-peg (remove-peg board jumped) p1 p2)))

(defn can-move?
  "Do any of the pegged positions have valid moves?"
  [board]
  (some (comp not-empty (partial valid-moves board))
        (map first (filter #(get (second %) :pegged) board))))

(def alpha-start 97)
(def alpha-end 123)
(def letters (map (comp str char) (range alpha-start alpha-end)))
(def pos-chars 3)

;; (defn render-pos
;;   [board pos]
;;   (str (nth letters (dec pos))
;;        (if (get-in board [pos :pegged])
;;          (colorize "0" :blue)
;;          (colorize "-" :red))))
;; (defn row-positions
;;   "Return all positions in the given row"
;;   [row-num]
;;   (range (inc (or (row-tri (dec row-num)) 0))
;;          (inc (row-tri row-num))))


;; (defn row-padding
;;   "String of spaces to add to the beginning of a row to center it"
;;   [row-num rows]
;;   (let [pad-length (/ (* (- rows row-num) pos-chars) 2)]
;;     (apply str (take pad-length (repeat " ")))))

;; (defn render-row
;;   [board row-num]
;;   (str (row-padding row-num (:rows board))
;;        (clojure.string/join " " (map (partial render-pos board)
;;                                      (row-positions row-num)))))

;; (defn print-board
;;   [board]
;;   (doseq [row-num (range 1 (inc (:rows board)))]
;;     (println (render-row board row-num))))

;; (defn letter->pos
;;   "Converts a letter string to the corresponding position number" 
;;   [letter]
;;   (inc (- (int (first letter)) alpha-start)))

;; (defn get-input
;;   "Waits for user to enter text and hit enter, then cleans the input"
;;   ([] (get-input nil))
;;   ([default]
;;    (let [input (clojure.string/trim (read-line))]
;;      (if (empty? input)
;;        default
;;        (clojure.string/lower-case input)))))

