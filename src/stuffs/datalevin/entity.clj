(ns stuffs.datalevin.entity
  (:require [datalevin.core :as d]
            [datalevin.impl.entity :as de]
            [mount.core :as mount :refer [defstate]]
            [potemkin.collections]
            [stuffs.mount :as smount]
            [stuffs.util :as su]
            [datalevin.db :as db]
            [clojure.string :as str]
            [clojure.set :as set])
  (:import (clojure.lang MapEntry)))

(declare entity ->Entity equiv-entity lookup-entity touch)

(defn- entid [db eid]
  (when (or (number? eid)
            (sequential? eid)
            (keyword? eid))
    (db/entid db eid)))

(defn entity [db eid]
  {:pre [(db/db? db)]}
  (when-let [e (entid db eid)]
    (->Entity db e (volatile! false) (volatile! {}) {} {})))

(defn- entity-attr [db a datoms]
  (if (db/multival? db a)
    (if (db/ref? db a)
      (reduce #(conj %1 (entity db (:v %2))) #{} datoms)
      (reduce #(conj %1 (:v %2)) #{} datoms))
    (if (db/ref? db a)
      (entity db (:v (first datoms)))
      (:v (first datoms)))))

(defn- -lookup-backwards [db eid attr not-found]
  (if-let [datoms (not-empty (db/-search db [nil attr eid]))]
    (if (db/component? db attr)
      (entity db (:e (first datoms)))
      (reduce #(conj %1 (entity db (:e %2))) #{} datoms))
    not-found))

(defprotocol Transactable
  (add [x attr v])
  (retract [x attr v])
  (->txs [x]))

(defn attr-types2 [db]
  (let [{ref-attrs  :db.type/ref
         many-attrs :db.cardinality/many
         components :db/isComponent} (:rschema db)]
    {:ref-attrs       (set/difference ref-attrs many-attrs)
     :ref-rattrs      (into #{} (map db/reverse-ref) components)
     :ref-many-rattrs (into #{} (map db/reverse-ref) (set/difference ref-attrs components))
     :ref-many-attrs  (set (:db.cardinality/many (:rschema db)))}))

(defn- e->txs [e]
  (let [eid (.eid e)
        {:keys [ref-attrs ref-rattrs ref-many-rattrs ref-many-attrs]} (attr-types2 (.db e))]
    (into
      []
      (mapcat (fn [[k [meta v]]]
                (let [;retract? (identical? v ::retract)
                      {:keys [op]} meta
                      #_#_e-kval (lookup-entity e k)]
                  (cond
                    (or (ref-many-attrs k)
                        (ref-many-rattrs k))
                    (case op
                      :assoc [[:db.fn/retractAttribute eid k]
                              {:db/id eid
                               k      (mapv (fn [e] (or (:db/id e) e)) (su/ensure-vec v))}]
                      :add [{:db/id eid
                             k      (mapv (fn [e] (or (:db/id e) e)) (su/ensure-vec v))}]
                      :dissoc [[:db.fn/retractAttribute eid k]]
                      :retract (into []
                                     (map (fn [e]
                                            [:db/retract eid k (:db/id e)]))
                                     (su/ensure-vec v)))

                    :else
                    (case op
                      (:dissoc :retract) [[:db.fn/retractAttribute eid k]]
                      (:assoc :add) [[:db/add eid k v]])))))
      (.tbd e))))

(potemkin.collections/def-map-type Entity [db eid touched cache tbd meta-map]
  (get [e k not-found]
    (or (some-> tbd (get k) second) (lookup-entity e k) not-found))
  (assoc [e k v]
    (assert (keyword? k) "attribute must be keyword")
    (Entity. db eid touched cache (assoc tbd k [{:op :assoc} v]) meta-map))
  (dissoc [e k]
    (assert (keyword? k) "attribute must be keyword")
    (Entity. db eid touched cache (assoc tbd k [{:op :dissoc}]) meta-map))
  (keys [e] (touch e) (keys @cache))
  (meta [e] meta-map)
  (with-meta [e meta]
    (Entity. db eid touched cache tbd meta))

  Transactable
  (add [e attr v]
    (assert (keyword? attr) "attribute must be keyword")
    (Entity. db eid touched cache (assoc tbd attr [{:op :add} v]) meta-map)
    )
  (retract [e attr v]
    (assert (keyword? attr) "attribute must be keyword")
    (Entity. db eid touched cache (assoc tbd attr [{:op :retract} v]) meta-map))
  (->txs [e]
    (e->txs e))

  Object
  (toString [e] (pr-str (assoc @cache :db/id eid)))
  (hashCode [e] (hash eid))                                 ; db?
  (equals [e o] (equiv-entity e o))


  clojure.lang.Seqable
  (seq [e] (touch e) (seq @cache))

  clojure.lang.Associative
  (equiv [e o] (equiv-entity e o))
  (containsKey [e k] (not= ::nf (lookup-entity e k ::nf)))
  (entryAt [e k] (some->> (lookup-entity e k) (clojure.lang.MapEntry. k)))
  ;
  (empty [e] (throw (UnsupportedOperationException.)))
  (cons [e [k v]]
    (assoc e k v))
  (count [e] (touch e) (count @(.-cache e)))
  )

(defn- get-stage [^Entity e]
  (not-empty (.tbd e)))

(defmethod print-method Entity [e, ^java.io.Writer w]
  (let [staged (get-stage e)
        ent    (assoc @(.cache e) :db/id (.eid e))]
    (.write w
            (pr-str
              (cond-> ent
                staged (assoc :<STAGED> staged))))))

(defn- equiv-entity [^Entity this that]
  (and
    (instance? Entity that)
    ;; (= db  (.-db ^Entity that))
    (= (.-eid this) (.-eid ^Entity that))))

(defn- lookup-entity
  ([this attr] (lookup-entity this attr nil))
  ([^Entity this attr not-found]
   (if (= attr :db/id)
     (.-eid this)
     (if (db/reverse-ref? attr)
       (-lookup-backwards (.-db this) (.-eid this) (db/reverse-ref attr) not-found)
       (if-some [v (@(.-cache this) attr)]
         v
         (if @(.-touched this)
           not-found
           (if-some [datoms (not-empty (db/-search (.-db this) [(.-eid this) attr]))]
             (let [value (entity-attr (.-db this) attr datoms)]
               (vreset! (.-cache this) (assoc @(.-cache this) attr value))
               value)
             not-found)))))))

(defn- datoms->cache [db datoms]
  (reduce (fn [acc part]
            (let [a (:a (first part))]
              (assoc acc a (entity-attr db a part))))
          {} (partition-by :a datoms)))


(defn entity? [x] (instance? Entity x))

(defn touch [^Entity e]
  {:pre [(entity? e)]}
  (when-not @(.-touched e)
    (when-let [datoms (not-empty (db/-search (.-db e) [(.-eid e)]))]
      (vreset! (.-cache e) (->> datoms
                                (datoms->cache (.-db e))
                                (de/touch-components (.-db e))))
      (vreset! (.-touched e) true)))
  e)

(defn- x->txs [x]
  (cond
    (entity? x) (->txs x)
    :else [x]))

(defn entities->txs [txs]
  (into []
        (mapcat x->txs)
        txs))

(defn transact! [conn txs]
  (d/transact! conn (entities->txs txs)))

(comment
  (do
    (def db-path "data/lab/entity-db")

    (def schema
      {:user/handle    #:db {:valueType :db.type/string
                             :unique    :db.unique/identity}
       :user/address   #:db{:valueType   :db.type/ref
                            :cardinality :db.cardinality/one}
       :address/street #:db{:valueType :db.type/string}
       :user/friends   #:db{:valueType   :db.type/ref
                            :cardinality :db.cardinality/many}})

    (defstate conn
      :start (d/create-conn
               db-path
               schema)
      :stop (d/close conn))

    (defn wipe-db! []
      (smount/with-restart
        ['conn]
        (su/delete-directory-recursive db-path)))

    ;(wipe-db!)
    (mount/start))
  ;(mount/stop)

  (transact! conn [{:user/handle  "ava"
                    :user/friends [{:user/handle "fred"}
                                   {:user/handle "jane"}]}])

  ;(touch (entity @conn [:user/handle "ava"]))
  ;; *** Simple example
  (let [ava-with-age (-> (entity @conn [:user/handle "ava"])
                         (assoc :user/age 42))]
    #spy/c (entities->txs [ava-with-age])
    ;; => [[:db/add 1 :user/age 42]]
    (transact! conn [ava-with-age])
    )


  ;; *** Nested entities must be transacted separately
  (let [{:keys [user/friends] :as ava}
        (update (entity @conn [:user/handle "ava"]) :user/age inc)
        fred   (some
                 #(when (= (:user/handle %) "fred") %)
                 friends)
        bestie (assoc fred :bestie? true)]
    #spy/c (entities->txs [ava bestie])
    ;; => [[:db/add 1 :user/age 43] [:db/add 2 :bestie? true]]
    (transact! conn [ava bestie]))

  ;; *** `add` and `retract` are directly defined on entity
  ;; they differ from assoc/dissoc in that they do not overwrite
  ;; the attr's values
  (let [ava  (entity @conn [:user/handle "ava"])
        fred (some
               #(when (= (:user/handle %) "fred") %)
               (:user/friends ava))]
    #spy/c (entities->txs [(retract ava :user/friends fred)])
    ;; => [[:db/retract 1 :user/friends 2]]
    (transact! conn [(retract ava :user/friends fred)])
    )

  )
;(require '[clojure.tools.namespace.repl :refer [refresh]])
;
;(refresh)
