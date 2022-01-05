(ns stuffs.log
  (:require [stuffs.ledger :as l]
            [mount.core :as mount]
            [spyscope.core]
            [tick.core :as t]
            [stuffs.util :as su]))

(declare transact-event! transact-events! get-events-asc get-events-desc get-instant-events-asc get-instant-events-desc)

(defn create-logger! [path]
  (doto
    (l/open path "log")
    (l/def-ledger-fns "log")))

(defn delete-logger! [path]
  (su/delete-directory-recursive path))

(defn log [x]
  (transact-event!
    (doto
      (if (map? x)
        x
        {:msg x})
      println)))

(defn logs
  "Takes either a start and end time/instant or a duration as a offset from now.
  Or a start time/instant and duration as offset."
  ([a b]
   (cond
     (and (number? a) (keyword? b))
     (let [offset (and
                    (t/<< (t/now) (t/new-duration a b)))]
       (get-instant-events-asc offset (t/now)))

     (and (su/date-like? a) (su/date-like? b))
     (get-instant-events-asc a b)
     :else (throw (ex-info (str "invalid arguments: " a b) {:args [a b]}))))
  ([start num unit]
   {:pre [(su/date-like? start) (number? num) (keyword? unit)]}
   (get-instant-events-asc (t/<< start (t/new-duration num unit)) start)))