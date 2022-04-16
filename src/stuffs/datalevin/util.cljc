(ns stuffs.datalevin.util)

(defn schema->unique-identities [schema]
  (into {}
        (filter (fn [[k v]]
                  (= :db.unique/identity (:db/unique v))))
        schema))

(defn schema->unique-identity-ks [schema]
  (not-empty
   (into #{}
         (keep (fn [[k v]]
                 (when (= :db.unique/identity (:db/unique v))
                   k)))
         schema)))
