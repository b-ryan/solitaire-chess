(ns solitaire-chess.core
  (:require [clojure.edn :as edn]))

(defn load-game [file]
  (edn/read-string (slurp file)))

(defn piece-symbol [piece]
  (case piece
    :rook "♜"
    :pawn "♟"
    :bishop "♝"
    :knight "♞"
    :queen "♛"
    :king "♚"))

(defn print-board [{:keys [board]}]
  (doseq [row [4 3 2 1]]
    (doseq [col [1 2 3 4]]
      (if-let [piece (get board [col row])]
        (print (piece-symbol piece))
        (print "_"))
      (print " "))
    (prn)))

(defn remove-nil [moves]
  (filter (fn [pos]
            (not (nil? pos)))
          moves))

(defn remove-invalid [moves]
  (filter (fn [[col row]]
            (and (>= col 1)
                 (<= col 4)
                 (>= row 1)
                 (<= row 4)
                 ))
          moves))

(defmulti all-moves (fn [piece pos] piece))

(defmethod all-moves :rook [_ [col row]]
  (reduce (fn [moves x]
            (merge moves
                   (if (not (= col x)) [x row])
                   (if (not (= row x)) [col x])))
          []
          (range 1 5)))

(defmethod all-moves :pawn [_ [col row]]
  (let [prev-col (- col 1)
        next-col (+ col 1)
        next-row (+ row 1)]
    [[prev-col next-row]
     [next-col next-row]]))

(defmethod all-moves :bishop [_ [col row]]
  (reduce (fn [moves x]
            (if (not (= x 0))
              (merge moves
                     [(+ col x) (+ row x)]
                     [(+ col x) (- row x)])
              moves))
          []
          (range -3 4)))

(defmethod all-moves :knight [_ [col row]]
  [[(+ col 3) (+ row 2)]
   [(+ col 3) (- row 2)]
   [(- col 3) (+ row 2)]
   [(- col 3) (- row 2)]
   [(+ col 2) (+ row 3)]
   [(+ col 2) (- row 3)]
   [(- col 2) (+ row 3)]
   [(- col 2) (- row 3)]])

(defmethod all-moves :queen [_ pos]
  (apply merge
         (all-moves :rook pos)
         (all-moves :bishop pos)))

(defmethod all-moves :king [_ [col row]]
  (for [x [-1 0 1]
        y [-1 0 1]
        :when (not (and (= x 0) (= y 0)))]
    [(+ col x) (+ row y)]))

(defn remove-non-captures [board moves]
  (filter #(get board %)
          moves))

(defn valid-moves [{:keys [board]} pos]
  (->> (all-moves (get board pos) pos)
       remove-nil
       remove-invalid
       (remove-non-captures board)))

(valid-moves {:board {[1 2] :king
                      [2 2] :pawn
                      [2 1] :bishop}}
             [1 2])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  )
