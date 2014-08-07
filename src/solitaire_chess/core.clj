(ns solitaire-chess.core
  (:require [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]))

(defn load-board [file]
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
  [[(+ col 2) (+ row 1)]
   [(+ col 2) (- row 1)]
   [(- col 2) (+ row 1)]
   [(- col 2) (- row 1)]
   [(+ col 1) (+ row 2)]
   [(+ col 1) (- row 2)]
   [(- col 1) (+ row 2)]
   [(- col 1) (- row 2)]])

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

(defn valid-moves [board pos]
  (->> (all-moves (get board pos) pos)
       remove-nil
       remove-invalid
       (remove-non-captures board)))

(defn move-piece [board from-pos to-pos]
  (-> board
      (assoc to-pos (get board from-pos))
      (dissoc from-pos)))

(defn possible-moves
  [board]
  (apply concat
         (for [from-pos (keys board)]
           (reduce (fn [ret to-pos]
                     (merge ret {:move [from-pos to-pos]
                                 :board (move-piece board from-pos to-pos)}))
                   []
                   (valid-moves board from-pos)))))


(defn prepend-all
  [pre all]
  (map #(cons pre %)
       all))

(defn all-games
  [board]
  (apply concat
         (for [move (possible-moves board)]
           (let [sub-paths (all-games (:board move))]
             (if (seq sub-paths)
               (prepend-all move sub-paths)
               [[move]])))))

(defn winning-games
  [board]
  (filter #(= 1 (-> % last :board count))
          (all-games board)))

(defn pos->str
  [[col row]]
  (str (get {1 "a"
             2 "b"
             3 "c"
             4 "d"} col)
       row))

(defn print-game
  [board moves]
  (loop [board board
         moves moves]
    (if (seq moves)
      (let [move (first moves)
            from-pos (first (:move move))
            to-pos (second (:move move))
            piece (get board from-pos)]
        (prn (str "Move " (name piece)
                  " from " (pos->str from-pos)
                  " to " (pos->str to-pos)))
        (recur (:board move)
               (rest moves))))
    ))

;(pprint (all-games {[1 1] :king
;                    [1 2] :pawn
;                    [1 3] :pawn
;                    [1 4] :pawn}))
;
;(pprint (all-games {[1 2] :king
;                    [2 2] :queen
;                    [3 3] :knight}))

(let [board {[1 2] :king
             [2 2] :queen
             [3 3] :knight}]
  (print-game board (first (winning-games board))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  )
