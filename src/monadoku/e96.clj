(ns monadoku.e96
  (:require [monadoku.async :refer [do-puzzle print-grid]]))

(defn- raw-puzzle-to-vec [[_ digit-string]]
  (vec (map #(Integer/parseInt %) (re-seq #"\d" digit-string))))


(defn- slurp-puzzles []
  (let [all (slurp (clojure.java.io/reader (clojure.java.io/resource "p096_sudoku.txt")))
        raw-puzzles (re-seq #"Grid\s+\d+\n((?:\d{9}\n){9})" all)
        puzzles (vec (map raw-puzzle-to-vec raw-puzzles))]
    puzzles))

(let [start (System/currentTimeMillis)
      puzzles (slurp-puzzles)
      answers (vec (map-indexed #(do-puzzle %2 %1) puzzles))
      corners (vec (map #(+ (* 100 (get % 0)) (* 10 (get % 1)) (get % 2)) answers))]
  (println "Corners" corners (apply + corners) (- (System/currentTimeMillis) start)))


(do-puzzle monadoku.puzzles/hardest "hardest")

;(let [puzzles (slurp-puzzles)
;      puzzle (nth puzzles 7)]
;  (print-grid puzzle)
;  (do-puzzle puzzle "euler 5th (6th)"))

(Thread/sleep 5000)
