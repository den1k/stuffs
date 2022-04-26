(ns stuffs.route
  (:require [lambdaisland.uri :as uri]
            [medley.core :as md]
            #?@(:cljs [[reitit.frontend :as rf]
                       [reitit.frontend.easy :as rfe]])))

(defn nil-or-empty? [x]
  (or (nil? x)
      (and (coll? x) (empty? x))))

(defn current-m []
  #?(:cljs
     (let [url
           (-> js/window .-location)
           {:keys [data path-params query-params]}
           (rf/match-by-path (:router @rfe/history) url)]
       {:name         (:name data)
        :path-params  path-params
        :query-params query-params})))

(defn current []
  #?(:cljs
     (let [{:keys [name path-params query-params]} (current-m)]
       [name path-params query-params])))

(defn push-state
  ([k] (push-state k nil nil))
  ([k params] (push-state k params nil))
  ([k params query]
   #?(:cljs (rfe/push-state
              k
              (md/remove-vals nil-or-empty? params)
              (md/remove-vals nil-or-empty? query))
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
