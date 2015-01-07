(ns monadoku.async
  (:require [monadoku.puzzles :as puzzles]
            [clojure.core.async :refer [chan <! >! >!! alt!! timeout pub sub unsub go go-loop close!]])
  (:use clojure.test))

(derive ::Cell ::Participant)
(derive ::Container ::Participant)
(derive ::Grid ::Participant)

(derive ::Row ::Container)
(derive ::Column ::Container)
(derive ::Box ::Container)

(defn ptype [p & _rest] (first (:name p)))


(def ^:dynamic *printer-chan* (chan 5))
(go-loop []
  (let [[fn arg] (<! *printer-chan*)]
    (apply fn arg)
    (recur)))
(defn log [& more] (>!! *printer-chan* [print more]))
(defn logln [& more] (>!! *printer-chan* [println more]))
(defn debug [& more]
  #_(let [tmp (apply print-str more)
        m (re-find #"(Cell\s+64|Box\s+2)\b" tmp)]
    ;(println "foo" m tmp)
    (when m
      (>!! *printer-chan* [println more]))))

(defn boom [_a err] (debug "boom! " err))


(defn print-grid [grid]
  (let [sep (apply str (repeat 17 "-"))
        grid-lines (apply str (map #(apply println-str %) (partition 9 grid)))]
    (logln (str sep "\n" grid-lines sep))))


(defn row-for-cell [cell]
  (quot cell 9))


(defn col-for-cell [cell]
  (mod cell 9))


(defn box-for-cell [cell]
  (+ (quot cell 27) (* 3 (quot (mod cell 9) 3))))


(defn make-channel [] (chan))


(defn make-participant [state publication]
  (let [c (make-channel)]
    (sub publication (:name state) c)
    ;(println "made" (:name state) (ptype state))
    (go-loop [state state]
      (let [msg (<! c)
            nstate ((:fn msg) state msg)]
        (if (nil? nstate)
          (do (unsub publication (:name state) c) (close! c))
          (recur nstate))))))


(defn make-cell [index grid publisher publication]
  (let [name [::Cell index]
        state {:name       name :publisher publisher
               :grid       grid :possibilities (set (range 1 10))
               :containers [[::Row (row-for-cell index)] [::Column (col-for-cell index)] [::Box (box-for-cell index)]]}]
    (make-participant state publication)
    name))


(defn make-container [ptype index publisher publication cells-seq]
  (let [name [ptype index]
        cells (into #{} cells-seq)
        state {:name name :publisher publisher :cells cells
               :possibleCellsForValue (apply hash-map (interleave (range 1 10) (repeat cells)))}]
    ;(logln "make" name cells)
    (make-participant state publication)
    name))


(defn make-grid []
  (let [name [::Grid 0]
        publisher (chan 81920)
        publication (pub publisher #(:name %))
        cells (vec (map #(make-cell % name publisher publication) (range 81)))
        rows (vec (map #(make-container ::Row %1 publisher publication %2) (range 9) (partition 9 cells)))
        cols (vec (map #(make-container ::Column %1 publisher publication %2) (range 9) (apply map vector (partition 9 cells))))
        boxes (vec (map #(make-container ::Box %1 publisher publication (map vec (partition 2 %2))) (range 9) (->> (partition 3 cells)
                                                                                                                   (partition 3)
                                                                                                                   (apply interleave)
                                                                                                                   (partition 3)
                                                                                                                   (map flatten))))
        result (go (let [c (make-channel)]
                     (sub publication :result c)
                     (<! c)))]
    (make-participant {:name name :grid (vec (repeat 81 0)) :result result :publisher publisher} publication)
    [publisher result]))


(defn tell! [publisher recpt msg-fn val sender]
  (debug "(tell)" sender " telling " recpt msg-fn val sender)
  (go
    (>! publisher {:name recpt :fn msg-fn :val val :sender sender})))


(defn tell-all! [publisher recipients sender msg val]
  (dorun (map #(tell! publisher % msg val sender) recipients)))


(defmulti is-value ptype)
(defmulti is-not-value ptype)


(defmethod is-value ::Cell [{:keys [name containers grid publisher] :as cell} {:keys [val sender]}]
  (debug name "has been set to value" val sender)
  (tell-all! publisher containers name is-value val)
  (tell! publisher grid is-value val name)
  (assoc (dissoc cell :possibilities) :value val)
  ;nil
  )


(defmethod is-not-value ::Cell [{:keys [name containers possibilities publisher] :as cell} {:keys [val sender]}]
  (let [remaining (disj possibilities val)]
    (debug name "has been told is-not-value" val remaining (count remaining) (:value cell) "by" sender containers)
    (tell-all! publisher (remove #(= sender %) containers) name is-not-value val)
    (if (= (count remaining) 1)
      (is-value cell {:val (first remaining) :sender nil})
      (if (> (count remaining) 1)
        (assoc-in cell [:possibilities] remaining)
        cell))))


(defn remove-possibility-for-value [possibilities cell val {:keys [name publisher]}]
  (let [updated (remove #(= cell %) possibilities)]
    (debug name "removing" cell "as possibility for" val (count updated) updated)
    (when (= (count updated) 1)
      (debug name "noticed" (first updated) "only candidate for" val)
      (tell! publisher (first updated) is-value val name))
    updated))


(defmethod is-value ::Container [{:keys [name cells possibleCellsForValue publisher] :as container} {:keys [val sender]}]
  (debug name "was told is-value " sender val cells)
  (tell-all! publisher (disj cells sender) name is-not-value val)
  (reduce (fn [c ind]
            (update-in c [:possibleCellsForValue ind] remove-possibility-for-value sender ind container))
          (-> container
              (update-in [:cells] disj sender)
              (assoc-in [:possibleCellsForValue val] #{})
              )
          (disj (set (keys possibleCellsForValue)) val)))


(defmethod is-not-value ::Container [container {:keys [val sender]}]
  (debug (:name container) "was told is-not-value" sender val)
  (update-in container [:possibleCellsForValue val] remove-possibility-for-value sender val container))


(defmethod is-value ::Grid [grid {:keys [val sender]}]
  ;(logln (:name grid) "was told is-value" sender val (:count grid))
  (let [r (assoc-in grid [:grid (last sender)] val)
        zeros (count (filter zero? (:grid r)))]
    ;(print-grid (:grid r))
    (when (= zeros 0)
      (>!! (:result grid) (:grid r)))
    r))


(defn apply-puzzle [puzzle publisher]
  (go-loop [ind 0
            puzzle puzzle]
    (when-let [val (first puzzle)]
      (when-not (zero? val)
        (debug "start telling " ind " val is " val)
        (tell! publisher [::Cell ind] is-value val nil))
      (recur (inc ind) (rest puzzle)))))

(defn do-puzzle [puzzle name]
  (let [start (System/currentTimeMillis)
        [publisher result] (make-grid)]
    (apply-puzzle puzzle publisher)
    (alt!!
      (timeout 10000) ([v c] (log name "Timed out" c v))
      result ([r _]
               (logln name "Total time: " (- (System/currentTimeMillis) start))
               (print-grid r)
               r))))


;;; Use the following in the repl to test
;(do-puzzle puzzles/fiendish "fiendish")
;(do-puzzle puzzles/hardest "hardest")
;(print-grid puzzles/euler-example)
;(do-puzzle puzzles/euler-example "euler")

; Hrmm could be useful
(dorun (->> (ns-publics 'monadoku.puzzles)
            (map last)
            (map #(vector (var-get %) %))
            (map #(apply do-puzzle %))))

(Thread/sleep 2500)
