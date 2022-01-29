(ns stuffs.missionary
  (:require [missionary.core :as m]
            [com.rpl.specter :as sp]
            [net.cgrand.xforms :as x])
  (:import (clojure.lang IDeref)))

(defrecord MissionaryTask [task]
  IDeref
  (deref [_]
    (m/? task)))

(prefer-method print-method java.util.Map clojure.lang.IDeref)

(defmacro blocking [& body]
  `(->MissionaryTask (m/via m/blk ~@body)))

(defmacro cpu-bound [& body]
  `(->MissionaryTask (m/via m/cpu ~@body)))

(defn task? [x]
  (= MissionaryTask (type x)))

(def walker
  (sp/recursive-path
    [] p
    (sp/if-path map?
                [sp/ALL
                 (sp/if-path [sp/LAST task?]
                             [(sp/collect-one sp/FIRST) sp/LAST]
                             [(sp/collect-one sp/FIRST) sp/LAST p])]
                (sp/if-path sequential?
                            [sp/INDEXED-VALS
                             (sp/if-path [sp/LAST task?]
                                         sp/FIRST
                                         [(sp/collect-one sp/FIRST) sp/LAST p])]))))

(defn conc-coll
  "Concurrent Map
  Recursively finds and runs tasks in vals concurrently,
  leaving other values untouched."
  [coll]
  (let [paths
        (some->> (sp/select walker coll)
                 not-empty
                 (mapv #(cond-> % (not (vector? %)) vector)))
        [paths tasks]
        (x/transjuxt [(comp (map butlast) (x/into []))
                      (comp (map last) (x/into []))]
                     paths)
        run-tasks
        (when paths
          (apply m/join
                 (fn [& completed-tasks]
                   (reduce (fn [out [p ct]]
                             (assoc-in out p ct))
                           coll
                           (map vector paths completed-tasks)))
                 (map :task tasks)))]
    (if run-tasks
      (m/? run-tasks)
      coll)))

(comment
  (time
    (conc-coll {:a (blocking (do (Thread/sleep 300) :a))
                :b [{:c (blocking (do (Thread/sleep 300) :123))}]})))
