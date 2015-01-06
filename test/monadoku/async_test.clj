(ns monadoku.async-test
  (:require [monadoku.async])
  (:use clojure.test))

(deftest hierarchy-test
  (testing "Hierarchy"
    (is (isa? :monadoku.async/Cell :monadoku.async/Participant))
    (is (isa? :monadoku.async/Row :monadoku.async/Participant))
    (is (isa? :monadoku.async/Row :monadoku.async/Container))
    ))

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
    (is (= 1 (box-for-cell 3)))
    (is (= 2 (box-for-cell 6)))
    (is (= 2 (box-for-cell 8)))
    (is (= 2 (box-for-cell 26)))
    (is (= 3 (box-for-cell 27)))
    (is (= 6 (box-for-cell 72)))
    (is (= 8 (box-for-cell 80))))
  (testing "Boxes")
  )


;; these tests are clearly incomplete! haven't come up with good way to
;; get results from final agent, not sure if it's worth the effort

(run-tests)
