(ns monadoku.async-test
  (:require [monadoku.puzzles :as puzzles])
  (:use monadoku.async monadoku.common)
  (:use clojure.test))


(deftest euler-example-test
  (is (= puzzles/euler-example-soln (do-puzzle puzzles/euler-example "euler-example"))))

(deftest other-examples-test
  (is (= true (correct? (do-puzzle puzzles/easy "easy"))))
  (is (= true (correct? (do-puzzle puzzles/mild "mild"))))
  (is (= true (correct? (do-puzzle puzzles/difficult1 "difficult1"))))
  (is (= true (correct? (do-puzzle puzzles/difficult25 "difficult25"))))
  (is (= true (correct? (do-puzzle puzzles/fiendish  "fiendish"))))
  (is (= true (correct? (do-puzzle puzzles/hardest "hardest"))))
  )

