(ns stuffs.route
  (:require [lambdaisland.uri :as uri]
            [medley.core :as md]
            #?@(:cljs [[reitit.frontend :as rf]
                       [reitit.frontend.easy :as rfe]])))

(defn push-state
  ([k] (push-state k nil nil))
  ([k params] (push-state k params nil))
  ([k params query]
   #?(:cljs (rfe/push-state
              k
              (md/remove-vals empty? params)
              (md/remove-vals empty? query))
      :clj  (constantly nil))))

(defn kv-join [sep m]
  (transduce
    (comp
      (mapcat (fn [[k v]] [(name k) v]))
      (interpose sep))
    str
    m))

(defn href
  ([path-params] (href path-params nil))
  ([path-params query-params]
   (str
     "/"
     (kv-join "/" path-params)
     (when (not-empty query-params)
       (str "?" (uri/map->query-string query-params))))))
