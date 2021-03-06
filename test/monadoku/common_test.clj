(ns monadoku.common-test
  (:require [monadoku.puzzles :as puzzles])
  (:use monadoku.common)
  (:use clojure.test))

(deftest hierarchy-test
  (testing "Hierarchy"
    (is (isa? :monadoku/Cell :monadoku/Participant))
    (is (isa? :monadoku/Row :monadoku/Participant))
    (is (isa? :monadoku/Row :monadoku/Container))
    ))

(deftest grid-predicates-test
  (is (= false (complete? puzzles/euler-example)))
  (is (= true (complete? puzzles/euler-example-soln)))
  (is (= true (correct? puzzles/euler-example-soln)))
  (is (= false (correct? (vec (repeat 81 1)))))
  )

(deftest container-for-cell-tests
  (testing "Rows"
    (is (= 0 (row-for-cell 0)))
    (is (= 0 (row-for-cell 8)))
    (is (= 2 (row-for-cell 26)))
    (is (= 3 (row-for-cell 27)))
    (is (= 8 (row-for-cell 72)))
    (is (= 8 (row-for-cell 80))))
  (testing "Cols"
    (is (= 0 (col-for-cell 0)))
    (is (= 3 (col-for-cell 3)))
    (is (= 8 (col-for-cell 8)))
    (is (= 8 (col-for-cell 26)))
    (is (= 0 (col-for-cell 27)))
    (is (= 0 (col-for-cell 72)))
    (is (= 8 (col-for-cell 80))))
  (testing "Boxes"
    (is (= 0 (box-for-cell 0)))
    (is (= 3 (box-for-cell 3)))
    (is (= 6 (box-for-cell 6)))
    (is (= 6 (box-for-cell 8)))
    (is (= 6 (box-for-cell 26)))
    (is (= 1 (box-for-cell 27)))
    (is (= 2 (box-for-cell 72)))
    (is (= 2 (box-for-cell 64)))
    (is (= 8 (box-for-cell 80))))
  )
