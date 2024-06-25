(ns stuffs.http
  (:require [stuffs.util :as su]
            [org.httpkit.client :as http]
            [clojure.core.memoize :as mem]))

;(defonce get-mem-ttl (su/memoize-ttl (comp deref http/get) (* 4 su/hour-ms)))
;(defonce request-mem-ttl (su/memoize-ttl (comp deref http/request) (* 4 su/hour-ms)))

(do
  ;; see https://github.com/http-kit/http-kit#enabling-client-sni-support-disabled-by-default
  ;; Needs Java >= 8, http-kit >= 2.4.0-alpha6
  (require '[org.httpkit.sni-client :as sni-client])
  ;; Change default client for your whole application:
  (alter-var-root #'org.httpkit.client/*default-client* (fn [_] sni-client/default-client)))

(defn request->json [opts]
  (let [{:as resp :keys [status body]} @(http/request opts)]
    (case status
      (200 201) (some-> body su/read-json-keywordized)
      (throw (ex-info "Request failed" resp)))))

(defonce request->json-mem-ttl (su/memoize-ttl request->json (* 4 su/hour-ms)))

(defn clear-request-ttl-cache! []
  (mem/memo-clear! request->json-mem-ttl)
  :cache-cleared)

(def ^:dynamic *http-request->json-mem-ttl* request->json-mem-ttl)

(defn without-mem* [thunk]
  (binding [*http-request->json-mem-ttl* request->json]
    (thunk)))

(defmacro without-mem [& body]
  `(without-mem* (fn [] ~@body)))

(comment
  (mem/snapshot request->json-mem-ttl)
  (clear-request-ttl-cache!))

(def ^:dynamic *http-request->json* request->json)

(defn with-mem* [thunk]
  (binding [*http-request->json* request->json-mem-ttl]
    (thunk)))

(defmacro with-mem [& body]
  `(with-mem* (fn [] ~@body)))
