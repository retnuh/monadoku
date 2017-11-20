(ns monadoku.async
  (:require [monadoku.puzzles :as puzzles]
            [clojure.core.async :refer [chan <! >! >!! alt!! alt! timeout pub sub unsub mult tap go go-loop close! sliding-buffer]])
  (:use clojure.test monadoku.common))


(def ^:dynamic *printer-chan* (chan 5000))
(go-loop []
  (let [[fn arg] (<! *printer-chan*)]
    (apply fn arg)
    (recur)))
(defn warn [& more] (>!! *printer-chan* [print more]))
(defn warnln [& more] (>!! *printer-chan* [println more]))
(defn log [& more] #_(>!! *printer-chan* [print more]))
(defn logln [& more] #_(>!! *printer-chan* [println more]))
(defn debug [& more]
  #_(let [tmp (apply print-str more)
        m (re-find #"(Cell\s+64|Box\s+2)\b" tmp)]
    ;(println "foo" m tmp)
    (when true
      (>!! *printer-chan* [println more]))))

(defn boom [_a err] (debug "boom! " err))


(defn print-grid [grid]
  (let [sep (apply str (repeat 17 "-"))
        grid-lines (apply str (map #(apply println-str %) (partition 9 grid)))]
    (logln (str sep "\n" grid-lines sep))))


(defn make-channel [] (chan 100))


(defn make-participant [state publication]
  (let [c (make-channel)]
    (sub publication (:name state) c)
    ;(println "made" (:name state) (ptype state))
    (go-loop [state state]
      (let [msg (<! c)
            nstate ((:fn msg) state msg)]
        (recur nstate)))))


(defn make-cell [index grid publisher publication]
  (let [name [:monadoku/Cell index]
        state {:name       name :publisher publisher
               :grid       grid :possibilities (set (range 1 10))
               :containers [[:monadoku/Row (row-for-cell index)] [:monadoku/Column (col-for-cell index)] [:monadoku/Box (box-for-cell index)]]}]
    (make-participant state publication)
    name))


(defn make-container [ptype index publisher publication cells-seq]
  (let [name [ptype index]
        cells (set cells-seq)
        state {:name name :publisher publisher :cells cells
               :possibleCellsForValue (apply hash-map (interleave (range 1 10) (repeat cells)))}]
    ;(logln "make" name cells)
    (make-participant state publication)
    name))


(defmulti is-value ptype)
(defmulti is-not-value ptype)
(defmulti push-state ptype)
(defmulti pop-state ptype)


(defn fn-name [msg-fn]
  (cond (= msg-fn is-value) "is-value"
        (= msg-fn is-not-value) "is-not-value"
        (= msg-fn push-state) "push-state"
        (= msg-fn pop-state) "pop-state"))


(defn tell! [publisher recpt msg-fn val sender]
  (debug "(tell)" sender " telling " recpt (fn-name msg-fn) val sender)
  (>!! publisher {:name recpt :fn msg-fn :val val :sender sender}))


(defn tell-all! [publisher recipients sender msg val]
  (dorun (map #(tell! publisher % msg val sender) recipients)))


(defn basic-push-state [state]
  (let [stack (get state :stack [])
        sans-stack (dissoc state :stack)
        saved (conj stack sans-stack)]
    (assoc sans-stack :stack saved)))

;(defmethod push-state :monadoku/Grid [{:keys [publisher cells name rows cols boxes] :as state} _]
;  (tell-all! publisher cells name push-state nil)
;  (tell-all! publisher rows name push-state nil)
;  (tell-all! publisher cols name push-state nil)
;  (tell-all! publisher boxes name push-state nil)
;  (basic-push-state state))


(defmethod push-state :monadoku/Participant [state _]
  (basic-push-state state))


(defmethod pop-state :monadoku/Participant [{stack :stack name :name} _]
  #_(logln name "has been reset to state" (last stack))
  (assoc (last stack) :stack (pop stack)))


(defmethod is-value :monadoku/Cell [{:keys [name containers grid publisher] :as cell} {:keys [val sender]}]
  (if (:value cell)
    cell
    (do
      (logln name "has been set to value" val sender)
      (tell-all! publisher containers name is-value val)
      (tell! publisher grid is-value val name)
      (assoc (dissoc cell :possibilities) :value val))
    ))


(defmethod is-not-value :monadoku/Cell [{:keys [name containers possibilities publisher] :as cell} {:keys [val sender]}]
  (if-not (:value cell)
    (let [remaining (disj possibilities val)]
      (debug name "has been told is-not-value" val remaining (count remaining) (:value cell) "by" sender containers)
      (tell-all! publisher (remove #(= sender %) containers) name is-not-value val)
      (if (= (count remaining) 1)
        (do
          (logln name "has only one remaining value" (first remaining))
          (is-value cell {:val (first remaining) :sender nil}))
        (assoc-in cell [:possibilities] remaining)))
    cell
    )
  )


(defn guess [{:keys [name possibilities] :as cell} {:keys [val sender]}]
  (logln "guess" name possibilities val sender (:value cell) (Thread/currentThread))
  (if (>= val (count possibilities))
    (do (logln "cannot guess, exhausted possibilities") cell)
    (let [g (nth (vec possibilities) val)]
      (logln name "guessing value" g val possibilities sender)
      (is-value cell {:val g :sender sender})
      )))


(defn remove-possibility-for-value [possibilities cell val {:keys [name publisher]}]
  (let [updated (set (remove #(= cell %) possibilities))]
    (if (contains? possibilities cell)
      (debug name "removing" cell "as possibility for" val (count updated) updated))
    (when (= (count updated) 1)
      (logln name "noticed" (first updated) "only candidate for" val)
      (tell! publisher (first updated) is-value val name))
    updated))


(defmethod is-value :monadoku/Container [{:keys [name cells possibleCellsForValue publisher] :as container} {:keys [val sender]}]
  (debug name "was told is-value " sender val cells)
  (tell-all! publisher (disj cells sender) name is-not-value val)
  (reduce (fn [c ind]
            (update-in c [:possibleCellsForValue ind] remove-possibility-for-value sender ind container))
          (-> container
              (update-in [:cells] disj sender)
              (assoc-in [:possibleCellsForValue val] #{}))
          (disj (set (keys possibleCellsForValue)) val)))


(defmethod is-not-value :monadoku/Container [container {:keys [val sender]}]
  (debug (:name container) "was told is-not-value" sender val)
  (update-in container [:possibleCellsForValue val] remove-possibility-for-value sender val container))


(defmethod is-value :monadoku/Grid [grid {:keys [val sender]}]
  (logln (:name grid) "was told is-value" sender val (:count grid))
  (let [r (assoc-in grid [:grid (last sender)] val)]
    #_(print-grid (:grid r))
    (>!! (:updates grid) (:grid r))
    r))


(defn make-guess [{:keys [cells rows cols boxes publisher name] :as grid-state} grid-vec stack]
  (let [unknown (ffirst (drop-while #(pos? (last %)) (map-indexed vector grid-vec)))
        s (conj stack [unknown 0 grid-vec])]
    (logln "Pushing state and making guess for" [:monadoku/Cell unknown] (map butlast s) grid-state)
    (tell! publisher name push-state nil nil)
    (tell-all! publisher cells nil push-state nil)
    (tell-all! publisher rows nil push-state nil)
    (tell-all! publisher cols nil push-state nil)
    (tell-all! publisher boxes nil push-state nil)
    (tell! publisher [:monadoku/Cell unknown] guess 0 :guess)
    s))


(defn pop-all [{:keys [cells rows cols boxes publisher name] :as grid-state}]
  (tell! publisher name pop-state nil :guess)
  (tell-all! publisher cells nil pop-state nil)
  (tell-all! publisher rows nil pop-state nil)
  (tell-all! publisher cols nil pop-state nil)
  (tell-all! publisher boxes nil pop-state nil))


(defn guess-again [{:keys [cells rows cols boxes publisher name] :as grid-state} stack]
  (let [bottom (pop stack)
        [cell poss-index grid-vec] (last stack)
        s (conj bottom [cell (inc poss-index) grid-vec])]
    (logln "Guessing again" name [:monadoku/Cell cell] (inc poss-index) (map butlast s) grid-state)
    (pop-all grid-state)
    (tell! publisher name push-state nil :guess)
    (tell-all! publisher cells nil push-state nil)
    (tell-all! publisher rows nil push-state nil)
    (tell-all! publisher cols nil push-state nil)
    (tell-all! publisher boxes nil push-state nil)
    (tell! publisher [:monadoku/Cell cell] guess (inc poss-index) :guess)
    s))


(defn make-grid []
  (let [name [:monadoku/Grid 0]
        publisher (chan 81920)
        publisher-mult (mult publisher)
        spigot (tap publisher-mult (chan (sliding-buffer 1)))
        publication (pub (tap publisher-mult (chan 81920)) #(:name %))
        cells (vec (map #(make-cell % name publisher publication) (range 81)))
        rows (vec (map #(make-container :monadoku/Row %1 publisher publication %2) (range 9) (partition-rows cells)))
        cols (vec (map #(make-container :monadoku/Column %1 publisher publication %2) (range 9) (partition-cols cells)))
        boxes (vec (map #(make-container :monadoku/Box %1 publisher publication (map vec (partition 2 %2))) (range 9) (partition-boxes cells)))
        result (make-channel)
        updates (make-channel)
        grid {:name  name :grid (vec (repeat 81 0)) :updates updates :publisher publisher
              :cells cells :rows rows :cols cols :boxes boxes}]
    (make-participant grid publication)
    (go-loop [prev (vec (repeat 81 0))
              guess-stack []]
      (alt!
        updates ([g _]
                  (logln "grid updated" (correct? g) (complete? g) (map butlast guess-stack))
                  (print-grid g)
                  (cond
                    (correct? g) (>! result g)
                    (complete? g) (recur g (guess-again grid guess-stack))
                    :else (recur g guess-stack)
                    ))
        spigot (recur prev guess-stack)
        (timeout 5) (do
                      (logln "internal time out, checking prev update" (correct? prev) (complete? prev) (map butlast guess-stack))
                      (cond
                        (not (complete? prev)) (recur prev (make-guess grid prev guess-stack))
                        (not (correct? prev)) (do
                                                (pop-all grid)
                                                (recur prev (guess-again grid (pop guess-stack))))))))
    [publisher result]))


(defn apply-puzzle [puzzle publisher]
  (go-loop [ind 0
            puzzle puzzle]
    (when-let [val (first puzzle)]
      (when-not (zero? val)
        (debug "start telling " ind " val is " val)
        (tell! publisher [:monadoku/Cell ind] is-value val nil))
      (recur (inc ind) (rest puzzle)))))

(defn do-puzzle [puzzle name & [print?]]
  (let [start (System/currentTimeMillis)
        [publisher result] (make-grid)]
    (apply-puzzle puzzle publisher)
    (alt!!
      (timeout 10000) ([v c] (warnln name "Timed out" c v) nil)
      result ([r _]
              (when print?
                (warnln name "Total time: " (- (System/currentTimeMillis) start))
                (print-grid r))
               r))))


;;; Use the following in the repl to test
;(do-puzzle puzzles/fiendish "fiendish")
;(do-puzzle puzzles/hardest "hardest")
;(print-grid puzzles/euler-example)
;(do-puzzle puzzles/euler-example "euler")

; Hrmm could be useful
;(dorun (->> (ns-publics 'monadoku.puzzles)
;            (map last)
;            (map #(vector (var-get %) %))
;            (map #(apply do-puzzle %))))

;(Thread/sleep 2500)
