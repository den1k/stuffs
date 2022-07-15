(ns stuffs.datalevin.util
  (:require [datalevin.core :as d]
            [datalevin.entity :as de]
            [datalevin.db :as ddb]
            [medley.core :as md]
            [stuffs.util :as su]
            [clojure.set :as set]
            [datalevin.storage :as s]
            [net.cgrand.xforms :as x])
  (:import (datalevin.storage Store)
           (datalevin.db DB)))

(def base-entity-schema
  {:id         {:db/unique    :db.unique/identity
                :db/valueType :db.type/string}
   :created-at {:db/valueType :db.type/instant}
   :updated-at {:db/valueType :db.type/instant
                :db/index     true}})

(def reverse-ref? ddb/reverse-ref?)
(def reverse-ref ddb/reverse-ref)
(def mem-reverse-ref (memoize ddb/reverse-ref))
(def ref? ddb/ref?)

(defn- rschema->attr-types
  [{ref-attrs  :db.type/ref
    many-attrs :db.cardinality/many
    components :db/isComponent}]
  (let [components (set components)]
    {:component-attrs components
     :ref-attrs       (set (set/difference ref-attrs many-attrs))
     :ref-rattrs      (into #{} (map reverse-ref) components)
     :ref-many-rattrs (into #{} (map reverse-ref) (set/difference ref-attrs components))
     :ref-many-attrs  (set/intersection ref-attrs (set many-attrs))}))

(defn schema
  "Return the schema"
  [db]
  (s/schema ^Store (.-store ^DB db)))

(defn rschema
  "Return the rschema"
  [db]
  (s/rschema ^Store (.-store ^DB db)))

(def db->attr-types
  (let [deduped-rschema->attr-types (su/dedupe-f rschema->attr-types)]
    (fn attr-types [db]
      (deduped-rschema->attr-types (rschema db)))))

(def db-attrs->types
  (let [f (su/dedupe-f
            (fn [attr-types]
              (into {}
                    (mapcat (fn [[type attrs]]
                              (map vector attrs (repeat type))))
                    attr-types)))]
    (fn attrs->type [db]
      (f (db->attr-types db)))))

(defn db->unique-idents [db]
  (:db.unique/identity (rschema db)))

(defn find-entity [db {:as ent-map :keys [db/id]}]
  (if id
    (d/entity db id)
    (let [unique-idents (db->unique-idents db)]
      (some #(d/entity db (find ent-map %)) unique-idents))))

(defn find-entities [db ent-map]
  (let [unique-idents (db->unique-idents db)]
    (into [] (map #(d/entity db (find ent-map %))) unique-idents)))

(defn find-entities-id->map [db ent-map]
  (let [unique-idents (db->unique-idents db)]
    (into {}
          (comp (keep #(find ent-map %))
                (keep (fn [ref] (d/entity db ref)))
                (map (fn [e] [(:db/id e) e]))) unique-idents)))

(defn find-conflicting-entities [db ent-map]
  (let [unique-idents (db->unique-idents db)]
    (not-empty
      (sequence
        (comp (keep #(find ent-map %))
              (keep (fn [ref] (d/entity db ref)))
              (remove #(= (:db/id %) (:db/id ent-map))))
        unique-idents))))

(defn ents->rattr-refs [db ents]
  (let [{:keys [ref-rattrs
                ref-many-rattrs]} (db->attr-types db)
        all-rattrs (into ref-many-rattrs ref-rattrs)]
    (sequence
      (x/for [e _
              a all-rattrs
              :let [[a v :as ref] (find e a)]
              :when ref]
        [a (cond
             (contains? ref-rattrs a) (:db/id v)
             (contains? ref-many-rattrs a) (mapv :db/id v))])
      ents)))

(defn- map-refs [f ent-map db]
  (let [{:as   at
         :keys [component-attrs ref-attrs ref-rattrs ref-many-rattrs ref-many-attrs]} (db->attr-types db)]
    (letfn [(on-ent-map [x]
              (cond
                (de/entity? x) (:db/id x)
                (map? x) (f db x)
                :else x))]
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

(declare existing-entity)

(defn add-timestamps [{:as m :keys [created-at]}]
  (let [inst (su/date-instant)]
    (cond-> m
      (nil? created-at) (assoc :created-at inst)
      true (assoc :updated-at inst))))

(defn- prep-entity-for-transact [db {:as ent-map db-id :db/id} {:as opts :keys [idk]}]
  (let [id   (idk ent-map)
        inst (su/date-instant)
        {:keys [created-at] db-id :db/id exist-id idk} (cond db-id (existing-entity db db-id)
                                                             id (d/entity db [idk id]))]
    (-> (map-refs (fn [db ent-map] (prep-entity-for-transact db ent-map opts)) ent-map db)
        (assoc
          :db/id (or db-id (tempid-gen))
          idk (or id exist-id (su/id-gen))
          :created-at (or created-at inst)
          :updated-at inst))))

(defn- prep-entity-for-transact2 [db {:as ent-map db-id :db/id} {:as opts}]
  (let [inst (su/date-instant)
        {:keys [created-at] db-id :db/id} (find-entity db ent-map)]
    (-> (map-refs (fn [db ent-map] (prep-entity-for-transact2 db ent-map opts)) ent-map db)
        (assoc
          :db/id (or db-id (tempid-gen))
          :created-at (or created-at inst)
          :updated-at inst))))

(defn make-transact-entity!
  "Transacts and returns an entity against an immutable DB value"
  ([conn] (make-transact-entity! conn {:idk :id}))
  ([conn {:as opts :keys [idk]}]
   (fn tx-ent!
     ([ent-map] (tx-ent! conn [] ent-map))
     ([txs ent-map] (tx-ent! conn txs ent-map))
     ;; could use schema to figure out unique identity keys if there is no db/id
     ;; throw otherwise
     ([conn txs ent-map]
      (let [{db-id :db/id :as prepped-ent} (prep-entity-for-transact @conn ent-map opts)
            {:keys [db-after tempids]} (->> (or txs [])
                                            (into [prepped-ent])
                                            (d/transact! conn))
            eid (if (neg-int? db-id)
                  (get tempids db-id)
                  db-id)]
        (d/touch (d/entity db-after eid)))))))

(defn make-transact-entity2!
  "Transacts and returns an entity against an immutable DB value"
  ([conn] (make-transact-entity2! conn {}))
  ([conn {:as   opts
          :keys [wrap-upsert-error]
          :or   {wrap-upsert-error su/identity-thunk}}]
   (fn tx-ent!
     ([ent-map] (tx-ent! conn [] ent-map))
     ([txs ent-map] (tx-ent! conn txs ent-map))
     ;; could use schema to figure out unique identity keys if there is no db/id
     ;; throw otherwise
     ([conn txs ent-map]
      (let [{db-id :db/id :as prepped-ent}
            (prep-entity-for-transact2 @conn ent-map opts)

            txs (or txs [])

            {:keys [db-after tempids]}
            (try (->> txs
                      (into [prepped-ent])
                      (d/transact! conn))
                 (catch Throwable e
                   ;; merge on upsert error
                   (println ::error e)
                   (when (-> (ex-data e)
                             :error
                             (= :transact/upsert))
                     (wrap-upsert-error
                       #(let [db                         @conn
                              conflicting-ents           (find-conflicting-entities db ent-map)
                              conf-retractions           (into []
                                                               (map (fn [{:keys [db/id]}]
                                                                      [:db/retractEntity id]))
                                                               conflicting-ents)
                              merged-ent                 (into prepped-ent
                                                               cat
                                                               conflicting-ents)
                              merged-ent-with-rattr-refs (into merged-ent
                                                               (ents->rattr-refs db conflicting-ents))]
                          (d/transact! conn conf-retractions)
                          (->> txs
                               (into [merged-ent-with-rattr-refs])
                               (d/transact! conn)))))))
            eid (if (neg-int? db-id)
                  (get tempids db-id)
                  db-id)]
        (d/touch (d/entity db-after eid)))))))

(defn make-transact-entities!
  "Transacts entities against an immutable DB value
  Retracts attrs with `nil` values"
  ([conn] (make-transact-entities! conn {:idk :id}))
  ([conn opts]
   (fn tx-ents!
     ([ent-maps] (tx-ents! conn [] ent-maps))
     ([txs ent-maps] (tx-ents! conn txs ent-maps))
     ;; could use schema to figure out unique identity keys if there is no db/id
     ;; throw otherwise
     ([conn txs ent-maps]
      (let [db           @conn
            prepped-ents (mapcat
                           (comp entity-retract-nils-txs
                                 #(prep-entity-for-transact db % opts))
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
        )))))

(defn make-transact-entities2!
  "Transacts entities against an immutable DB value
  Retracts attrs with `nil` values"
  ([conn] (make-transact-entities2! conn {}))
  ([conn opts]
   (fn tx-ents!
     ([ent-maps] (tx-ents! conn [] ent-maps))
     ([txs ent-maps] (tx-ents! conn txs ent-maps))
     ;; could use schema to figure out unique identity keys if there is no db/id
     ;; throw otherwise
     ([conn txs ent-maps]
      (let [db           @conn
            prepped-ents (mapcat
                           (comp entity-retract-nils-txs
                                 #(prep-entity-for-transact2 db % opts))
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
        )))))

(defn existing-entity [db eid]
  (when (and (ddb/entid db eid)
             (not-empty (d/datoms db :eavt eid)))
    (d/entity db eid)))

(defn make-datoms [conn]
  (fn datoms
    ([]
     (fn datoms-fn [& args]
       (apply d/datoms @conn args)))
    ([& args]
     (apply d/datoms @conn args))))

(defn make-entity
  ([conn] (make-entity conn false))
  ([conn touch?]
   (fn entity
     ([]
      (let [db @conn]
        (fn entity-fn [eid]
          (entity db eid))))
     ([eid]
      (entity @conn eid))
     ([db eid]
      (let [ref (cond
                  (and (vector? eid) (su/keyword-identical? :db/id (first eid))) (second eid)
                  (de/entity? eid) (:db/id eid)
                  (d/datom? eid) (:e eid)
                  :else eid)]
        (cond-> (existing-entity db ref)
          touch? (some-> d/touch)))))))

(defn make-datoms->entities [conn]
  (let [datoms (make-datoms conn)
        entity (make-entity conn)]
    (fn datoms->entities
      ([index]
       (datoms->entities index nil :e))
      ([index attr]
       (datoms->entities index attr :e))
      ([index attr index-k]
       {:pre [(contains? #{:e :v} index-k)]}
       (sequence
         (comp (map index-k)
               (keep entity))
         (if-not attr
           (datoms index)
           (datoms index attr)))))))


(defn query->map [q]
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
    (d/q query db d/entity)))

(defn make-q [conn]
  (fn [q & args]
    (apply d/q q @conn args)))

(defn make-q-entity [conn]
  (fn [q]
    (q-entity @conn q)))

(defn where-entity [db where-clauses]
  (q-entity db (into [:where] where-clauses)))

(defn make-where-entity [conn]
  (fn [where-clauses]
    (where-entity @conn where-clauses)))