(ns monadoku.agents-test
  (:require [monadoku.agents])
  (:use clojure.test))

(deftest hierarchy-test
  (testing "Hierarcy"
    (is (isa? :monadoku.agents/Cell :monadoku.agents/Participant))
    (is (isa? :monadoku.agents/Row :monadoku.agents/Participant))
    (is (isa? :monadoku.agents/Row :monadoku.agents/Container))
    ))

(run-tests)
