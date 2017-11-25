(ns monadoku.trampoline
  (:require [monadoku.puzzles :as puzzles]
            [clojure.core.async :as async])
  (:use [monadoku.common])
  (:use clojure.test))

(defn logln [& more] (apply println more))
(defn info [& more] #_(apply println more))
(defn debug [& more] #_(apply println more))

(defn print-grid [grid]
  (let [sep (apply str (repeat 17 "-"))
        grid-lines (apply str (map #(apply println-str %) (partition 9 (map #(or % 0) grid))))]
    (logln (str sep "\n" grid-lines sep))))

(defn boom [_a err] (logln "boom! " err))

(defn make-cell [index grid] {:name [:monadoku/Cell index] :possibilities (set (range 1 10)) :containers #{} :grid (:name grid)})
(defn make-container [ptype index] {:name [ptype index]
                                    :cells #{}
                                    :possibleCellsForValue (apply hash-map (interleave (range 1 10)
                                                                                       (repeat #{})))})

(defn add-container [cell container-name]
  ;(debug "adding container: " (:name cell) " " (:name @container-agent))
  (update-in cell [:containers] conj container-name))

(defn add-cell [container cell-name]
  (reduce (fn [c ind] (update-in c [:possibleCellsForValue ind] conj cell-name))
          (update-in container [:cells] conj cell-name)
          (range 1 10)))

(defn bind-cell [container cell]
  ;;(debug "binding cell: " (:name @container) " " (:name @cell))
  [(add-container cell (:name container))
   (add-cell container (:name cell))])

(defn bind-cells [container cells]
  (let [container-name (:name container)]
    (reduce (fn [participants cell] (let [[ncell ncontainer] (bind-cell (get participants container-name) cell)]
                                     (assoc participants (:name cell) ncell container-name ncontainer)))
            {(:name container) container}
            cells)))

(defn reduce-cells [initial-participants containers seq-of-cells]
  (reduce (fn [participants [container cells]]
            (let [cell-names (map :name cells)]
              (merge participants (bind-cells container (vals (select-keys participants cell-names))))))
          initial-participants
          (map vector containers seq-of-cells)))

(defn make-grid []
  (let [grid {:name [:monadoku/Grid 0] :remaining (into #{} (range 81)) :vals (sorted-map)}
        cells (vec (map #(make-cell % grid) (range 81)))
        rows (vec (map #(make-container :monadoku/Row %) (range 9)))
        cols (vec (map #(make-container :monadoku/Column %) (range 9)))
        boxes (vec (map #(make-container :monadoku/Box %) (range 9)))]
    (-> (into {(:name grid) grid} (map (juxt :name identity) cells))
        (reduce-cells rows (partition-rows cells))
        (reduce-cells cols (partition-cols cells))
        (reduce-cells boxes (partition-boxes cells)))))

(defn tell [recpt msg val sender]
  ;(debug sender " telling " recpt msg val sender)
  [recpt msg val sender])

(defn tell-all [recipients sender msg val]
  (mapv #(tell % msg val sender) recipients))

(defmulti is-value ptype)
(defmulti is-not-value ptype)
(defmulti msg-dedup-form ffirst)

(defmethod msg-dedup-form :monadoku/Cell [[cell-name msg val sender]]
  [cell-name msg val])

(defmethod msg-dedup-form :monadoku/Container [msg]
  msg)


(defmethod is-value :monadoku/Grid [{:keys [name remaining vals] :as grid} val sender]
  (debug name "has been set to value" val "at index" (second sender) "by" sender)
  [[]
   (assoc grid :remaining (disj remaining val) :vals (assoc vals (last sender) val))])


(defmethod is-value :monadoku/Cell [{:keys [name containers grid] :as cell} val sender]
  (debug name "has been set to value" val "by" sender)
  [(concat [(tell grid is-value val name)]
           (tell-all containers name is-value val))
   (assoc (dissoc cell :possibilities) :value val)])


(defmethod is-not-value :monadoku/Cell [{:keys [name containers possibilities] :as cell} val sender]
  (let [remaining (disj possibilities val)]
    (debug name "has been told is-not-value" val remaining "by" sender containers)
    (let [[msgs new-cell] (cond
                            (= (count remaining) 1) (is-value cell (first remaining) name)
                            (> (count remaining) 1) [[] (assoc-in cell [:possibilities] remaining)]
                            :else [[] cell])]
      [(concat (tell-all containers name is-not-value val) msgs)
       new-cell])))


(defn remove-possibility-for-value [possibilities cell val container-name]
  #_(logln "rpfv:" possibilities cell val container-name)
  (let [updated (disj possibilities cell) 
        msg (when (and (= (count updated) 1) (> (count possibilities) 1))
              (debug container-name "noticed" (first updated) "only candidate for" val)
              (tell (first updated) is-value val container-name))]
    #_(debug container-name "removing" cell "as possibility for" val (count updated) updated)
    [(if msg [msg] []) updated]))


(defmethod is-value :monadoku/Container [{:keys [name cells possibleCellsForValue] :as container} val sender]
  (debug name "was told is-value " sender val)
  (let [msgs (tell-all (disj cells sender) name is-not-value val)
        updated-container (-> container
                              (update-in [:cells] disj sender)
                              (update-in [:possibleCellsForValue] dissoc val))]
    (reduce (fn update-possibilities [[m c] ind]
              (let [[nm nposs] (remove-possibility-for-value (get-in c [:possibleCellsForValue ind]) sender ind name)]
                [(concat m nm) (assoc-in c [:possibleCellsForValue ind] nposs)]))
            [msgs updated-container]
            (disj (set (keys possibleCellsForValue)) val))))


(defmethod is-not-value :monadoku/Container [container val sender]
  (debug (:name container) "was told is-not-value" sender val)
  (let [[msgs possible-cells] (remove-possibility-for-value (get-in container [:possibleCellsForValue val]) sender
                                                            val (:name container))]
    [msgs
     (assoc-in container [:possibleCellsForValue val] possible-cells)]))


(defn extract-grid [grid]
  (vec (vals (get-in grid [[:monadoku/Grid 0] :vals]))))


(defn puzzle-messages [puzzle]
  (into [] (mapcat
            (fn init-msgs [ind val]
              (when-not (zero? val)
                (debug "initializing" ind "to val" val)
                [(tell [:monadoku/Cell ind] is-value val "puzzle")]))
            (range 81)
            puzzle)))

(defn apply-puzzle [puzzle grid]
  (let [msgs (puzzle-messages puzzle)]
    (debug "initial-msgs" (empty? msgs) msgs)
    (loop [grid grid
           seen-msgs #{}
           msgs msgs
           msg-count 0
           dedup-count 0]
      #_(debug "about to dispatch:" (first msgs))
      (if (or (complete? (extract-grid grid)) (empty? msgs))
        (do
          (logln "seen-msgs" (count seen-msgs) "msg-count" msg-count "dedup-count" dedup-count "total" (+ msg-count dedup-count))
          grid)
        (let [[recpt-name f val sender :as msg] (first msgs)
              dedup-form (msg-dedup-form msg)
              seen? (contains? seen-msgs dedup-form)]
          (info "dispatching:" msg "from:" sender "seen:" seen?)
          (if-not seen?
            (let [recpt (grid recpt-name)
                  [nmsgs nrecpt] (f recpt val sender)
                  nseen-msgs (conj seen-msgs dedup-form)]
              #_(logln "new recpt:" recpt-name nrecpt)
              #_(logln "new msgs:" nmsgs)
              (recur (assoc grid recpt-name nrecpt) nseen-msgs (concat (rest msgs) nmsgs) (inc msg-count) dedup-count))
            (do
              #_(logln "dedup:" dedup-form msg)
              (recur grid seen-msgs (rest msgs) msg-count (inc dedup-count))))))
      )))


(defn do-puzzle [puzzle name & [print?]]
  (let [start (System/currentTimeMillis)
        grid (make-grid)
        ans (apply-puzzle puzzle grid)
        extracted (extract-grid ans)
        delta (- (System/currentTimeMillis) start)]
    (when print?
      (if (complete? extracted)
        (logln name "Total time: " delta)
        (logln name "Incomplete after: " delta))
      (print-grid extracted))
    extracted))


;;; Use the following in the repl to test
;;(do-puzzle puzzles/fiendish "fiendish" true)
;;(do-puzzle puzzles/hardest "hardest" true)
;;(do-puzzle puzzles/easy "easy" true)
;;(def g (do-puzzle puzzles/mild "mild" true))
