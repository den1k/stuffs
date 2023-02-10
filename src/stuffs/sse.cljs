(ns stuffs.sse
  (:require ["sse.js" :refer [SSE]]
            [clojure.set :as set]
            [stuffs.js-interop :as j]))

(defn sse
  "{:method
    :headers {}
    :body \"...\"}"
  [url opts cb]
  (let [source (new SSE url (clj->js (set/rename-keys opts {:body :payload})))]
    (j/call source :addEventListener "message" cb)
    (j/call source :addEventListener "error" (fn [e] (throw e)))
    (j/call source :stream)
    #(j/call source :close)))