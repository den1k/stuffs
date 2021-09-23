(ns stuffs.datalevin.util
  (:require [datalevin.core :as d]
            [datalevin.db :as ddb]
            [medley.core :as md]
            [stuffs.util :as su]
            [clojure.set :as set]))

(def base-entity-schema
  {:id         {:db/unique    :db.unique/identity
                :db/valueType :db.type/string}
   :created-at {:db/valueType :db.type/instant}
   :updated-at {:db/valueType :db.type/instant
                :db/index     true}})

(defn- rschema->attr-types
  [{ref-attrs  :db.type/ref
    many-attrs :db.cardinality/many
    components :db/isComponent}]
  (let [components (set components)]
    {:component-attrs components
     :ref-attrs       (set (set/difference ref-attrs many-attrs))
     :ref-rattrs      (into #{} (map ddb/reverse-ref) components)
     :ref-many-rattrs (into #{} (map ddb/reverse-ref) (set/difference ref-attrs components))
     :ref-many-attrs  (set many-attrs)}))

(def db->attr-types
  (let [deduped-rschema->attr-types (su/dedupe-f rschema->attr-types)]
    (fn attr-types [db]
      (deduped-rschema->attr-types (:rschema db)))))

(defn- map-refs [f ent-map db]
  (let [{:as   at
         :keys [component-attrs ref-attrs ref-rattrs ref-many-rattrs ref-many-attrs]} (db->attr-types db)]
    (letfn [(on-ent-map [x]
              (cond->> x (map? x) (f db)))]
      (into {}
            (map (fn [[k v :as entry]]
                   [k
                    (cond
                      (component-attrs k) v

                      (or (ref-many-attrs k)
                          (ref-many-rattrs k))
                      (mapv on-ent-map (su/ensure-vec v))

                      (or (ref-attrs k)
                          (ref-rattrs k))
                      (on-ent-map v)

                      :else v)]))
            ent-map))))


(defonce ^:private tempid-gen
  (let [neg-cnt (atom 0)]
    (fn []
      (swap! neg-cnt dec))))

(defn- entity-retract-nils-txs [{:as ent-map :keys [id]}]
  (reduce-kv
    (fn [txs k v]
      (cond-> txs
        (nil? v) (conj [:db.fn/retractAttribute [:id id] k])))
    [(md/remove-vals nil? ent-map)]
    ent-map))

(defn- prep-entity-for-transact [db {:keys [id] :as ent-map}]
  (let [tempid (tempid-gen)
        inst   (su/date-instant)
        {:keys [created-at] db-id :db/id} (when id (d/entity db [:id id]))]
    (-> (map-refs prep-entity-for-transact ent-map db)
        (assoc
          :db/id (or db-id tempid)
          :id (or id (su/id-gen))
          :created-at (or created-at inst)
          :updated-at inst))))

(defn make-transact-entity!
  "Transacts and returns an entity against an immutable DB value"
  [conn]
  (fn tx-ent!
    ([ent-map] (tx-ent! conn [] ent-map))
    ([txs ent-map] (tx-ent! conn txs ent-map))
    ;; could use schema to figure out unique identity keys if there is no db/id
    ;; throw otherwise
    ([conn txs ent-map]
     (let [{db-id :db/id :as prepped-ent} (prep-entity-for-transact @conn ent-map)
           {:keys [db-after tempids]} (->> (or txs [])
                                           (into [prepped-ent])
                                           (d/transact! conn))
           eid (if (neg-int? db-id)
                 (get tempids db-id)
                 db-id)]
       (d/touch (d/entity db-after eid))))))

(defn make-transact-entities!
  "Transacts entities against an immutable DB value
  Retracts attrs with `nil` values"
  [conn]
  (fn tx-ents!
    ([ent-maps] (tx-ents! conn [] ent-maps))
    ([txs ent-maps] (tx-ents! conn txs ent-maps))
    ;; could use schema to figure out unique identity keys if there is no db/id
    ;; throw otherwise
    ([conn txs ent-maps]
     (let [prepped-ents (mapcat
                          (comp entity-retract-nils-txs
                                (partial prep-entity-for-transact conn))
                          ent-maps)
           {:as tx-report :keys [db-after tempids]} (->> (or txs [])
                                                         (into prepped-ents)
                                                         (d/transact! conn))
           #_#_eid (if (neg-int? db-id)
                     (get tempids db-id)
                     db-id)]
       #_(d/touch (d/entity db-after eid))
       ;; todo return transacted entities
       tx-report
       ))))

(defn- existing-entity [db eid]
  (when (and (ddb/entid db eid)
             (not-empty (d/datoms db :eavt eid)))
    (d/entity db eid)))

(defn make-entity [conn]
  (fn entity
    [eid]
    (let [ref (cond-> eid
                (and (vector? eid) (su/keyword-identical? :db/id (first eid))) second)]
      (some-> (existing-entity @conn ref) d/touch))))

(defn- query->map [q]
  (cond
    (map? q) q
    (sequential? q) (datalevin.parser/query->map q)
    :else (throw (ex-info
                   "Query should be a vector or a map"
                   {:error :parser/query, :form q}))))

(defn q-entity [db query]
  (let [query (-> (query->map query)
                  (update :in #(if %
                                 (into '[$ ?entfn] %)
                                 '[$ ?entfn]))
                  (assoc :find '[[?ent ...]])
                  (update :where conj '[(?entfn $ ?e) ?ent]))]
    (println query)
    (d/q query db d/entity)))

(defn make-q-entity [conn]
  (fn [q]
    (q-entity @conn q)))

(defn where-entity [db where-clauses]
  (q-entity db (into [:where] where-clauses)))

(defn make-where-entity [conn]
  (fn [where-clauses]
    (where-entity @conn where-clauses)))