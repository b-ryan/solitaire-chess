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

(defmulti all-moves (fn [board pos piece] piece))

(defmethod all-moves :rook [board [col row] _]
  (reduce (fn [moves x]
            (merge moves
                   (if (not (= col x)) [x row])
                   (if (not (= row x)) [col x])))
          []
          (range 1 5)))

(defmethod all-moves :pawn [board [col row] _]
  (let [prev-col (- col 1)
        next-col (+ col 1)
        next-row (+ row 1)]
    [[prev-col next-row]
     [next-col next-row]]))

(defmethod all-moves :bishop [board [col row] _]
  (reduce (fn [moves x]
            (if (not (= x 0))
              (merge moves
                     [(+ col x) (+ row x)]
                     [(+ col x) (- row x)])
              moves))
          []
          (range -3 4)))

(defmethod all-moves :knight [board [col row] _]
  [[(+ col 2) (+ row 1)]
   [(+ col 2) (- row 1)]
   [(- col 2) (+ row 1)]
   [(- col 2) (- row 1)]
   [(+ col 1) (+ row 2)]
   [(+ col 1) (- row 2)]
   [(- col 1) (+ row 2)]
   [(- col 1) (- row 2)]])

(defmethod all-moves :queen [board pos _]
  (apply merge
         (all-moves board pos :rook)
         (all-moves board pos :bishop)))

(defmethod all-moves :king [board [col row] _]
  (for [x [-1 0 1]
        y [-1 0 1]
        :when (not (and (= x 0) (= y 0)))]
    [(+ col x) (+ row y)]))

(defn remove-nil [moves]
  (filter (fn [pos]
            (not (nil? pos)))
          moves))

(defn remove-out-of-bounds [moves]
  (filter (fn [[col row]]
            (and (>= col 1)
                 (<= col 4)
                 (>= row 1)
                 (<= row 4)
                 ))
          moves))

(defn remove-non-captures [moves board]
  (filter #(get board %)
          moves))

(defmulti remove-intersections (fn [moves board pos piece] piece))
(defmethod remove-intersections :default moves [moves board pos piece] moves)

(defn step [x y] (if (<= x y) 1 -1))

(defn in-between [x y]
  (let [s (step x y)]
    (range (+ x s) y s)))

(defmethod remove-intersections :bishop
  [moves board [pcol prow] _]
  (filter (fn [[mcol mrow]]
            (every? nil?
                  (for [c (in-between pcol mcol)
                        r (in-between prow mrow)]
                    (get board [c r]))))
          moves))

(defmethod remove-intersections :rook
  [moves board [pcol prow] _]
  (filter (fn [[mcol mrow]]
            (every? nil?
                    (if (= pcol mcol)
                      (for [r (in-between prow mrow)]
                        (get board [pcol r]))
                      (for [c (in-between pcol mcol)]
                        (get board [c prow])))))
          moves))

(defmethod remove-intersections :queen
  [moves board pos _]
  (-> moves
      (remove-intersections board pos :bishop)
      (remove-intersections board pos :rook)))

(defn valid-moves [board pos]
  (-> (all-moves board pos (get board pos))
      remove-nil
      remove-out-of-bounds
      (remove-non-captures board)
      (remove-intersections board pos (get board pos))))

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
               (rest moves))))))
(defn solve
  [board]
  (loop [games (winning-games board)]
    (if (seq games)
      (let [game (first games)
            remaining (rest games)]
        (print-game board game)
        (if (seq remaining)
          (do (prn) (prn "OR") (prn)
            (recur remaining)))))))

(defn -main
  [& args]
  (let [board (load-board "board.edn")]
    (solve board)))
