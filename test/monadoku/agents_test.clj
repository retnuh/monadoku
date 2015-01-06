(ns monadoku.agents-test
  (:require [monadoku.agents])
  (:use clojure.test))

(deftest hierarchy-test
  (testing "Hierarchy"
    (is (isa? :monadoku.agents/Cell :monadoku.agents/Participant))
    (is (isa? :monadoku.agents/Row :monadoku.agents/Participant))
    (is (isa? :monadoku.agents/Row :monadoku.agents/Container))
    ))

;; these tests are clearly incomplete! haven't come up with good way to
;; get results from final agent, not sure if it's worth the effort

(run-tests)
(shutdown-agents)
