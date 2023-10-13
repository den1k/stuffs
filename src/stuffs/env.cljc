(ns stuffs.env
  (:require
    #?@(:clj [[clojure.java.io :as io]
              [clojure.edn :as edn]
              [clojure.string :as str]])
    [clojure.core :as clj])
  #?(:cljs (:require-macros [stuffs.env :refer [get]]))
  (:refer-clojure :exclude [get]))

#?(:clj
   (defn- keywordize [s]
     (-> (str/lower-case s)
         (str/replace "_" "-")
         (str/replace "." "-")
         (keyword))))

#?(:clj
   (defn- sanitize-key [k]
     (let [s (keywordize (name k))]
       (if-not (= k s) (println "Warning: env key" k "has been corrected to" s))
       s)))

#?(:clj
   (defn- sanitize-val [k v]
     (if (string? v)
       v
       (do (println "Warning: env value" (pr-str v) "for key" k "has been cast to string")
           (str v)))))

#?(:clj
   (defn- read-system-env []
     (->> (System/getenv)
          (map (fn [[k v]] [(keywordize k) v]))
          (into {}))))
#?(:clj
   (defn- read-system-props []
     (->> (System/getProperties)
          (map (fn [[k v]] [(keywordize k) v]))
          (into {}))))

#?(:clj
   (defn- slurp-file [f]
     (when-let [f (io/file f)]
       (when (.exists f)
         (slurp f)))))

#?(:clj
   (defn- read-env-file [f]
     (when-let [content (slurp-file f)]
       (into {} (for [[k v] (edn/read-string content)]
                  [(sanitize-key k) (sanitize-val k v)])))))

#?(:clj
   (defn- warn-on-overwrite [ms]
     (doseq [[k kvs] (group-by key (apply concat ms))
             :let [vs (map val kvs)]
             :when (and (next kvs) (not= (first vs) (last vs)))]
       (println "Warning: env value" (first vs) "for key" k
                "has been overwritten with" (last vs)))))

#?(:clj
   (defn- merge-env [& ms]
     (warn-on-overwrite ms)
     (apply merge ms)))

#?(:clj
   (defn- read-env []
     (merge-env
       (read-env-file "env.edn")
       (read-system-env)
       (read-system-props))))

#?(:clj
   (defonce ^{:doc "A map of environment variables."}
     env (read-env)))

;; macro to inline in CLJS
(defmacro get [k]
  (clj/get env k))

(def dev-env (some-> (get :env) keyword))

(def dev? (= dev-env :dev))

(def prod? (= dev-env :prod))
