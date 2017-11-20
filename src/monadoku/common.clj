(ns monadoku.common)

(derive :monadoku/Cell :monadoku/Participant)
(derive :monadoku/Container :monadoku/Participant)

(derive :monadoku/Grid :monadoku/Container)
(derive :monadoku/Row :monadoku/Container)
(derive :monadoku/Column :monadoku/Container)
(derive :monadoku/Box :monadoku/Container)

(defn partition-rows [col]
  (partition 9 col))

(defn partition-cols [col]
  (apply map vector (partition 9 col)))

(defn partition-boxes [col]
  (->> (partition 3 col)
       (partition 3)
       (apply interleave)
       (partition 3)
       (map flatten)))

(defn complete? [grid]
  (not-any? zero? grid))

(defn correct-container? [container]
  (= (set (range 1 10)) (set container)))

(defn correct? [grid]
  (and (every? correct-container? (partition-rows grid))
       (every? correct-container? (partition-cols grid))
       (every? correct-container? (partition-boxes grid))))


(defn row-for-cell [cell]
  (quot cell 9))


(defn col-for-cell [cell]
  (mod cell 9))


(defn box-for-cell [cell]
  (+ (quot cell 27) (* 3 (quot (mod cell 9) 3))))

(defn ptype [p & _rest] (first (:name p)))

