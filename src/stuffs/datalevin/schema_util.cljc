(ns stuffs.datalevin.schema-util
  (:require [hyperfiddle.rcf :as rcf]))

(defn unique-identity? [{:as schema-val :keys [db/unique]}]
  (= unique :db.unique/identity))

(def filter-unique-identity-xf
  (filter (comp unique-identity? val)))

(defn schema->unique-identities [schema]
  (into {}
        filter-unique-identity-xf
        schema))

(rcf/tests
  (schema->unique-identities
    {:foo {:db/unique :db.unique/identity}
     :bar {:what :ever}})
  :=
  {:foo {:db/unique :db.unique/identity}})

(def filter-unique-attrs-xf
  (comp filter-unique-identity-xf
        (map (fn [[k _]] k))))

(defn schema->unique-attrs [schema]
  (not-empty
    (into #{}
          filter-unique-attrs-xf
          schema)))

(rcf/tests
  (schema->unique-attrs
    {:foo {:db/unique :db.unique/identity}
     :bar {:what :ever}})
  :=
  #{:foo})