(ns monadoku.agents-test
  (:require [monadoku.puzzles :as puzzles])
  (:use monadoku.agents)
  (:use clojure.test))

(deftest hierarchy-test
  (testing "Hierarchy"
    (is (isa? :monadoku.agents/Cell :monadoku.agents/Participant))
    (is (isa? :monadoku.agents/Row :monadoku.agents/Participant))
    (is (isa? :monadoku.agents/Row :monadoku.agents/Container))
    ))

(deftest grid-predicates-test
  (is (= false (complete? puzzles/euler-example)))
  (is (= true (complete? puzzles/euler-example-soln)))
  (is (= true (correct? puzzles/euler-example-soln)))
  (is (= false (correct? (vec (repeat 81 1)))))
  )

(deftest euler-example-test
  (is (= puzzles/euler-example-soln (do-puzzle puzzles/euler-example "euler-example"))))

(deftest other-examples-test
  (is (= true (correct? (do-puzzle puzzles/easy "easy"))))
  (is (= true (correct? (do-puzzle puzzles/mild "mild"))))
  (is (= true (correct? (do-puzzle puzzles/difficult1 "difficult1"))))
  (is (= true (correct? (do-puzzle puzzles/difficult25 "difficult25"))))
  (is (= true (correct? (do-puzzle puzzles/fiendish  "fiendish"))))
  ;; Bloody backtracking required?
  ;; Haven't implemented "fanciness" - mini-row/col cancelling, etc.
  #_(is (= true (correct? (do-puzzle puzzles/hardest "hardest"))))
  )
