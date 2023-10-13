(ns stuffs.http
  (:require [stuffs.util :as su]
            [org.httpkit.client :as http]))

;(defonce get-mem-ttl (su/memoize-ttl (comp deref http/get) (* 4 su/hour-ms)))
;(defonce request-mem-ttl (su/memoize-ttl (comp deref http/request) (* 4 su/hour-ms)))

(defn request->json [opts]
  (let [{:as resp :keys [status body]} @(http/request opts)]
    (case status
      (200 201) (some-> body su/read-json-keywordized)
      (throw (ex-info "Request failed" resp)))))

(defonce request->json-mem-ttl (su/memoize-ttl request->json (* 4 su/hour-ms)))

(def ^:dynamic *http-request->json* request->json)

(defn with-mem* [thunk]
  (binding [*http-request->json* request->json-mem-ttl]
    (thunk)))

(defmacro with-mem [& body]
  `(with-mem* (fn [] ~@body)))
