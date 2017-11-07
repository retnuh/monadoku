(ns monadoku.agents
  (:require [monadoku.puzzles :as puzzles]
            [clojure.core.async :as async])
  (:use clojure.test))

(derive ::Cell ::Participant)
(derive ::Container ::Participant)

(derive ::Grid ::Container)
(derive ::Row ::Container)
(derive ::Column ::Container)
(derive ::Box ::Container)

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


(def ^:dynamic *printer-agent* (agent nil))
(defn print-agent [_a fn & more] (apply fn more))
(defn log [& more] (send *printer-agent* print-agent print more))
(defn logln [& more] (send *printer-agent* print-agent println more))
(defn debug [& more] #_(send *printer-agent* print-agent println more))

(defn print-grid [grid]
  (let [sep (apply str (repeat 17 "-"))
        grid-lines (apply str (map #(apply println-str %) (partition 9 (map #(or % 0) grid))))]
    (logln (str sep "\n" grid-lines sep))))

(defn boom [_a err] (logln "boom! " err))

(defn make-cell [index counter] (agent {:name [::Cell index] :possibilities (set (range 1 10)) :counter counter}
                                       :error-handler boom ))
(defn make-container [ptype index] (agent {:name [ptype index]
                                           :possibleCellsForValue (apply hash-map (interleave (range 1 10)
                                                                                              (repeat #{})))}
                                          :error-handler boom))

(defn add-container [cell container-agent]
  ;(debug "adding container: " (:name cell) " " (:name @container-agent))
  (assoc-in cell [:containers (:name @container-agent)] container-agent))

(defn add-cell [container cell-agent]
  (reduce (fn [c ind] (update-in c [:possibleCellsForValue ind] conj cell-agent))
          (assoc-in container [:cells (:name @cell-agent)] cell-agent)
          (range 1 10)))

(defn bind-cell [container cell]
  ;(debug "binding cell: " (:name @container) " " (:name @cell))
  (send cell add-container container)
  (send container add-cell  cell))

(defn bind-cells [container-agent cell-agents]
  (dorun (map #(bind-cell container-agent %) cell-agents)))

(defn make-grid []
  (let [counter (agent (set (range 81)))
        cells (vec (map #(make-cell % counter) (range 81)))
        rows (vec (map #(make-container ::Row %) (range 9)))
        cols (vec (map #(make-container ::Column %) (range 9)))
        boxes (vec (map #(make-container ::Box %) (range 9)))]
    (dorun (map bind-cells rows (partition-rows cells)))
    (dorun (map bind-cells cols (partition-cols cells)))
    (dorun (map bind-cells boxes (partition-boxes cells)))
    {:name [::Grid 0] :cells cells :rows rows :cols cols :boxes boxes :counter counter}
    ))


(defn ptype [p & _rest] (first (:name p)))

(defn tell! [recpt msg val sender]
  ;(debug sender " telling " (:name @recpt) msg val sender)
  (send recpt msg val sender))

(defn tell-all! [recipients sender msg val]
  (dorun (map #(tell! (last %) msg val sender) recipients)))

(defmulti is-value ptype)
(defmulti is-not-value ptype)

(defmethod is-value ::Cell [{:keys [name containers counter] :as cell} val sender]
  (debug name "has been set to value" val "by" sender)
  (tell-all! containers name is-value val)
  (send counter disj (last name))
  (assoc (dissoc cell :possibilities) :value val))

(defmethod is-not-value ::Cell [{:keys [name containers possibilities] :as cell} val sender]
  (let [remaining (disj possibilities val)]
    (debug name "has been told is-not-value" val remaining "by" sender (keys containers))
    (tell-all! containers name is-not-value val)
    (if (= (count remaining) 1)
      (is-value cell (first remaining) nil)
      (if (> (count remaining) 1)
        (assoc-in cell [:possibilities] remaining)
        cell))))

(defn remove-possibility-for-value [possibilities cell val container-name]
  (let [updated (remove #(= cell (:name @%)) possibilities)]
    (debug container-name "removing" cell "as possibility for" val (count updated) (map #(:name @%) updated))
    (when (= (count updated) 1)
      (debug container-name "noticed" (:name @(first updated)) "only candidate for" val)
      (tell! (first updated) is-value val container-name))
    updated))

(defmethod is-value ::Container [{:keys [name cells possibleCellsForValue] :as container} val sender]
  (debug name "was told is-value " sender val)
  (tell-all! (dissoc cells sender) name is-not-value val)
  (reduce (fn [c ind]
            (update-in c [:possibleCellsForValue ind] remove-possibility-for-value sender ind name))
          (-> container
              (update-in [:cells] dissoc sender)
              (update-in [:possibleCellsForValue] dissoc val)
              )
          (disj (set (keys possibleCellsForValue)) val)))

(defmethod is-not-value ::Container [container val sender]
  (debug (:name container) "was told is-not-value" sender val)
  (update-in container [:possibleCellsForValue val] remove-possibility-for-value sender val (:name container)))

(defn apply-puzzle [puzzle grid]
  (dorun (map-indexed
           (fn [ind val]
             (when-not (zero? val)
               (debug "telling " ind " val is " val)
               (send (get-in grid [:cells ind]) is-value val nil)))
           puzzle)))

(defn extract-grid [grid]
  (vec (map (fn [a] (:value @a)) (:cells grid))))


;; We use the blocking functionality core.async here to get the final
;; answer back, to allow us to test things properly
(defn do-puzzle [puzzle name & [print?]]
  (let [start (System/currentTimeMillis)
        grid (make-grid)
        complete (async/chan 1)]
    (add-watch (:counter grid) :watcher
               (fn [_k _a _os ns]
                 (when (= (count ns) 0)
                   (remove-watch (:counter grid) :watcher)
                   (async/>!! complete (extract-grid grid))
                   (async/close! complete)
                   )))
    (apply-puzzle puzzle grid)
    (async/alt!!
      (async/timeout 10000) ([v c]
                             (when print?
                               (logln name "Timed out" c v)
                               (print-grid (extract-grid grid)))
                             nil)
      complete  ([r _]
                 (when print?
                   (logln name "Total time: " (- (System/currentTimeMillis) start))
                   (print-grid r))
                 r))))

;;; Use the following in the repl to test
;(do-puzzle puzzles/fiendish "fiendish" true)
;(do-puzzle puzzles/hardest "hardest" true)
;(do-puzzle puzzles/easy "easy" true)
;(do-puzzle puzzles/mild "mild" true)


; Hrmm could be useful
#_(dorun (->> (ns-publics 'monadoku.puzzles)
            (map last)
            (map #(vector (var-get %) %))
            (map #(apply do-puzzle %))))

