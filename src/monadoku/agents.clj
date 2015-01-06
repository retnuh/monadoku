(ns monadoku.agents
  (:require [monadoku.puzzles :as puzzles])
  (:use clojure.test))

(derive ::Cell ::Participant)
(derive ::Container ::Participant)

(derive ::Grid ::Container)
(derive ::Row ::Container)
(derive ::Column ::Container)
(derive ::Box ::Container)


(def ^:dynamic *printer-agent* (agent nil))
(defn print-agent [_a & more] (apply println more))
(defn debug [& more] #_(send *printer-agent* print-agent more))

(defn boom [_a err] (debug "boom! " err))

(defn make-cell [index counter] (agent {:name [::Cell index] :possibilities (set (range 1 10)) :counter counter} :error-handler boom ))
(defn make-container [ptype index] (agent {:name [ptype index]
                                           :possibleCellsForValue (apply hash-map (interleave (range 1 10) (repeat #{})))}
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
    (dorun (map bind-cells rows (partition 9 cells)))
    (dorun (map bind-cells cols (apply map vector (partition 9 cells))))
    (dorun (map bind-cells boxes (->> (partition 3 cells)
                               (partition 3)
                               (apply interleave)
                               (partition 3)
                               (map flatten))))
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

(defmethod is-value ::Cell [{:keys [name containers counter] :as cell} val _sender]
  (debug  name "has been set to value" val)
  (tell-all! containers name is-value val)
  (send counter disj (last name))
  (assoc (dissoc cell :possibilities) :value val))

(defmethod is-not-value ::Cell [{:keys [name containers possibilities] :as cell} val sender]
  (let [remaining (disj possibilities val)]
    (debug name "has been told is-not-value" val remaining (:value cell) "by" sender (keys containers))
    (tell-all! containers name is-not-value val)
    (if (= (count remaining) 1)
      (is-value cell (first remaining) nil)
      (if (> 1 (count remaining))
        (assoc-in cell [:possibilities] remaining)
        cell))))

(defn remove-possibility-for-value [possibilities cell val container-name]
  (let [updated (remove #(= cell (:name @%)) possibilities)]
    (debug container-name "removing" cell "as possibility for" val (count updated) (map #(:name @%) updated))
    (when (= (count updated) 1)
      (debug container-name "noticed" (:name @(first updated)) "only candidate for" val)
      (send (first updated) is-value val nil))
    updated))

(defmethod is-value ::Container [{:keys [name cells possibleCellsForValue] :as container} val sender]
  (debug name "was told is-value " sender val)
  (tell-all! (dissoc cells sender) name is-not-value val)
  (reduce (fn [c ind]
            (update-in c [:possibleCellsForValue ind] remove-possibility-for-value sender ind name))
          (-> container
              (update-in [:cells] dissoc sender)
              ;; hmm would have thought this would speed things up, but causes the harder ones (fiendish, hardest) to
              ;; fail
              #_(assoc-in [:possibleCellsForValue val] #{})
              )
          ;(disj (set
          (keys possibleCellsForValue)
          ;   )  val))
          ))

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

(defn print-grid [grid]
  (dorun (map #(apply println (map (fn [a] (:value @a)) %)) (partition 9 (:cells grid)))))

(defn do-puzzle [puzzle & return-grid]
  (let [start (System/currentTimeMillis)
        grid (make-grid)]
    (add-watch (:counter grid) :watcher
               (fn [_k _a _os ns]
                 (when (= (count ns) 0)
                   (remove-watch (:counter grid) :watcher)
                   (println "Total time: " (- (System/currentTimeMillis) start))
                   (print-grid grid))))
    (apply-puzzle puzzle grid)
    (and return-grid grid)))

;;; Use the following in the repl to test
(do-puzzle puzzles/fiendish)
(do-puzzle puzzles/hardest)


; Hrmm could be useful
;(->> (ns-publics 'monadoku.puzzles)
;     (map last)
;     (map var-get)
;     (map do-puzzle))
(Thread/sleep 1000)
(shutdown-agents)
