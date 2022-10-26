(ns stuffs.route
  (:require [lambdaisland.uri :as uri]
            [lambdaisland.uri.normalize :as uri.normalize]
            [clojure.string :as str]
            [medley.core :as md]
            [stuffs.js-interop :as j]
            [stuffs.util :as su]
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

(defn query-encode-k [k]
  (uri/query-encode
    (cond
      (simple-ident? k)
      (name k)
      (qualified-ident? k)
      (str (namespace k) "/" (name k))
      :else (str k))))

(defn query-encode-kv [k v]
  [(query-encode-k k)
   ;; encodes edn at the cost of
   ;; being more verbose ¯\_(ツ)_/¯
   (uri/query-encode (pr-str v))])

(defn query-decode-k [k]
  (when k (uri.normalize/percent-decode k)))

(defn query-decode-v [v]
  (when v (su/read-edn (uri.normalize/percent-decode (str/replace v #"\+" " ")))))

(defn get-query-k
  [k]
  #?(:cljs
     (let [url (js/URL. (j/get js/window :location))]
       (some-> (j/call-in url [:searchParams :get] (query-encode-k k))
               query-decode-v))))

(defn set-query-kv [k v]
  #?(:cljs
     (let [url (js/URL. (j/get js/window :location))
           [qk qv] (query-encode-kv k v)]
       (if false #_(and (string? v) (empty? v))
         (j/call-in url [:searchParams :delete] qk)
         (j/call-in url [:searchParams :set] qk qv))
       (j/call-in js/window [:history :pushState] #js {} "" url))))

(defn into-query [m]
  #?(:cljs
     (let [url (js/URL. (j/get js/window :location))]
       (doseq [[k v] m
               :let [[qk qv] (query-encode-kv k v)]]
         (j/call-in url [:searchParams :set] qk qv))
       (j/call-in js/window [:history :pushState] #js {} "" url))))
