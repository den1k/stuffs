(ns stuffs.ledger
  (:require [clj-uuid :as uuid]
            [tick.alpha.api :as t]
            [mount.core :as mount :refer [defstate]]
            [datalevin.core :as d]
            [stuffs.util :as su]
            [stuffs.mount :as smount])
  (:import (java.util Date)))

(def +subcounter-resolution+ 9999)
(deftype State [^short seqid ^long millis])

(defn make-monotonic-time [& _]
  (let [-state- (atom (->State 0 0))]
    (fn monotonic-time
      ([] (monotonic-time (System/currentTimeMillis)))
      ([inst-millis]
       (assert (>= inst-millis 0))
       (let [^State new-state
             (swap! -state-
                    (fn [^State current-state]
                      (loop [inst-millis inst-millis]
                        (if-not (= (.millis current-state) inst-millis)
                          (->State 0 inst-millis)
                          (let [tt (.seqid current-state)]
                            (if (< tt +subcounter-resolution+)
                              (->State (inc tt) inst-millis)
                              (recur (System/currentTimeMillis))))))))]
         (+ (.seqid new-state) 100103040000000000
            (* (+ 2208988800000 (.millis new-state)) 10000)))))))

(def mem-make-monotonic-time (memoize make-monotonic-time))

(def monotonic-time (make-monotonic-time))

(def ^:dynamic *kv-store* nil)
(def ^:dynamic *table* nil)
(def ^:dynamic *monotonic-time* monotonic-time)

(defn inst->monotonic [inst]
  (*monotonic-time* (.getTime inst)))

(defn monotonic->inst [mon]
  (Date. (clj-uuid.clock/posix-time mon)))

(defn instant->monotonic [instant]
  (*monotonic-time* (t/millis (t/between (t/epoch) instant))))

(defn monotonic->instant [mon]
  (t/instant (clj-uuid.clock/posix-time mon)))


(defn- t->monotonic [t]
  (when t
    (cond
      (su/date? t) (inst->monotonic t)
      (t/instant? t) (instant->monotonic t)
      (integer? t) (instant->monotonic (t/instant t)))))

(defn put-event-tx
  ([x]
   (when x
     (cond
       (map? x)
       (put-event-tx (*monotonic-time*) x)

       (vector? x)
       (let [[t v] x]
         (when (map? v)
           (if-let [mono (t->monotonic t)]
             (put-event-tx mono v)
             (println :ignoring-invalid-event (some-> x su/pretty-string))))))))
  ([k v] [:put *table* k v :long]))

(defn events-colls->txs [events]
  (into [] (keep put-event-tx) events))

(defn make-transact-event! [kv-store table]
  (fn [event]
    (binding [*table*          table
              *monotonic-time* (mem-make-monotonic-time kv-store table)]
      (when-let [ev (put-event-tx event)]
        (d/transact-kv kv-store [ev])))))

(defn make-transact-events! [kv-store table]
  (fn [events-coll]
    (binding [*table*          table
              *monotonic-time* (mem-make-monotonic-time kv-store table)]
      (d/transact-kv kv-store (events-colls->txs events-coll)))))

(defn make-get-events-asc [kv-store table]
  (fn get-events-asc
    ([]
     (d/get-range kv-store table [:all] :long))
    ([start-time end-time]
     (let [smono (t->monotonic start-time)
           emono (t->monotonic end-time)]
       (d/get-range kv-store table [:closed smono emono] :long)))))

(defn make-get-events-desc [kv-store table]
  (fn get-events-asc
    ([]
     (d/get-range kv-store table [:all-back] :long))
    ([start-time end-time]
     (let [smono (t->monotonic start-time)
           emono (t->monotonic end-time)]
       (d/get-range kv-store table [:closed-back emono smono] :long)))))

(defn make-get-instant-events-asc [get-events-asc]
  (comp
    #(sequence (map (fn [[k v]] [(monotonic->instant k) v])) %)
    get-events-asc))

(defn make-get-instant-events-desc [get-events-desc]
  (comp
    #(sequence (map (fn [[k v]] [(monotonic->instant k) v])) %)
    get-events-desc))

(defmacro def-ledger-fns [kv-store table]
  (assert kv-store)
  (assert table)
  (let [fns `((def ~'transact-event! (make-transact-event! ~kv-store ~table))
              (def ~'transact-events! (make-transact-events! ~kv-store ~table))
              (def ~'get-events-asc (make-get-events-asc ~kv-store ~table))
              (def ~'get-events-desc (make-get-events-desc ~kv-store ~table))
              (def ~'get-instant-events-asc (make-get-instant-events-asc ~'get-events-asc))
              (def ~'get-instant-events-desc (make-get-instant-events-desc ~'get-events-desc)))]
    (conj fns
          `(println (list (name 'declare) ~@(map (comp name second) fns)))
          `(println "Paste to declare ledger fns:")
          'do)))

;(macroexpand '(def-ledger-fns :foo :bar))
;(def-ledger-fns :foo :bar)

(defn open [path table]
  (let [l (d/open-kv path)]
    (run! #(d/open-dbi l %) [table])
    l))

(def close d/close-kv)

(comment
  (def ledger-path "data/datalevin/stuffs/ledger")
  (def ledger-v1-table "v1")

  (def ledger (open ledger-path ledger-v1-table))
  (transact-event! {:foo (rand-int 1000)})
  (transact-events! [{:foo (rand-int 1000)}])
  (get-instant-events-asc)
  (close ledger)
  (su/delete-directory-recursive ledger-path)

  )