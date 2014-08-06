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

(declare all-games)

(defn all-games-from-pos
  [board moves from-pos]
  (reduce (fn [ret to-pos]
            (let [new-board (move-piece board from-pos to-pos)]
              (all-games new-board
                         (merge ret {:move [from-pos to-pos]
                                     :board new-board }))))
          moves
          (valid-moves board from-pos)))

(defn all-games-from-pos
  [board from-pos]
  (for [to-pos (valid-moves board from-pos)]
    (let [new-board (move-piece board from-pos to-pos)
          move {:move [from-pos to-pos]
                :board new-board}]
      (prn "*****")
      (prn (all-games new-board))
      (map #(into [move] %)
           (all-games new-board))
      )))

(defn all-games-from-pos
  [board current-path from-pos]
  (for [to-pos ])



  (let [to-positions (valid-moves board from-pos)
        y (map (fn [to-pos]
                 (let [new-board (move-piece board from-pos to-pos)
                       new-path (merge current-path {:move [from-pos to-pos]
                                                     :board new-board})]
                   (all-games new-board new-path)
                   ))
               to-positions)]
    (prn "****")
    (pprint y)
    y
    ))

(defn all-games
  [board current-path]
  (let [sub-paths (map (partial all-games-from-pos board current-path)
                       (keys board))]
    (reduce #(apply merge % %2) current-path sub-paths)))

(pprint
  (all-games {[1 1] :king
              [1 2] :pawn
              [1 3] :pawn
              [1 4] :pawn}
             []))

(defn winning-games
  [board]
  (filter #(= 1 (count (:board %)))
          (all-games board)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  )
