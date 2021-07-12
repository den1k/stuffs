(ns stuffs.datalevin.util
  (:require [datalevin.core :as d]
            [datalevin.db :as ddb]
            [medley.core :as md]
            [stuffs.util :as su]
            [stuffs.util :as u]
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
  {:ref-attrs       (set/difference ref-attrs many-attrs)
   :ref-rattrs      (into #{} (map ddb/reverse-ref) components)
   :ref-many-rattrs (into #{} (map ddb/reverse-ref) (set/difference ref-attrs components))
   :ref-many-attrs  (set many-attrs)})

(def ^:private attr-types
  (let [deduped-rschema->attr-types (su/dedupe-f rschema->attr-types)]
    (fn attr-types [conn]
      (deduped-rschema->attr-types (:rschema conn)))))

(defn- map-refs [f ent-map]
  (let [{:keys [ref-attrs ref-rattrs ref-many-rattrs ref-many-attrs]} attr-types]
    (letfn [(on-ent-map [x]
              (cond-> x (map? x) f))]
      (into {}
            (map (fn [[k v :as entry]]
                   [k
                    (cond
                      (or (ref-many-attrs k)
                          (ref-many-rattrs k))
                      (mapv on-ent-map (su/ensure-vec v))

                      (or (ref-attrs k)
                          (ref-rattrs))
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

(defn- prep-entity-for-transact [conn {:keys [id] :as ent-map}]
  (let [tempid (tempid-gen)
        inst   (su/date-instant)
        {:keys [created-at] db-id :db/id} (when id (d/entity @conn [:id id]))]
    (-> (map-refs prep-entity-for-transact ent-map)
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
     (let [{db-id :db/id :as prepped-ent} (prep-entity-for-transact conn ent-map)
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