(ns stuffs.time
  (:require [stuffs.util :as su]
            [tick.core :as t]))

(defn forward-seq [start-t interval]
  (iterate #(t/>> % interval) start-t))

(defn backward-seq [start-t interval]
  (iterate #(t/<< % interval) start-t))

(defn days-seq
  ([] (days-seq (t/today)))
  ([start]
   (forward-seq start (t/new-period 1 :days))))

(defn future? [d]
  (t/> d (t/now)))

(defn future-times [start-t interval]
  (filter future? (forward-seq start-t interval)))

(def business-days
  #{t/MONDAY t/TUESDAY t/WEDNESDAY t/THURSDAY t/FRIDAY})

(defn last-date-of-day
  ([day] (last-date-of-day (t/today) day))
  ([start-t day]
   (su/ffilter #(= day (t/day-of-week %)) (drop 1 (backward-seq start-t (t/new-period 1 :days))))))

(defn next-date-of-day
  ([day] (next-date-of-day (t/today) day))
  ([start-t day]
   (su/ffilter #(= day (t/day-of-week %)) (drop 1 (forward-seq start-t (t/new-period 1 :days))))))
