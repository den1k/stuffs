(ns stuffs.util
  (:require [clojure.string :as str]
            [com.rpl.specter :as sp]
            [nano-id.core :as nano-id]
            [clojure.walk :as walk]
            [clojure.pprint :refer [pprint]]
            [hickory.core :as hic]
            [taoensso.encore :as enc]
            #?@(:clj  [[clojure.java.io :as io]
                       [clojure.data.csv :as csv]
                       [jsonista.core :as json]
                       [clojure.core.memoize :as mem]]
                :cljs [[goog.functions :as gfns]
                       [cljs.core :as cljs]
                       [stuffs.impl.partial :as partial]])
            [medley.core :as md]
            [tick.alpha.api :as t])
  (:refer-clojure :exclude [#?(:cljs keyword-identical?) partial])
  #?(:clj (:import (java.util Date)))
  #?(:cljs (:require-macros [stuffs.util])))

(defn project
  ([f coll] (project {} f coll))
  ([to f coll] (into to (map f) coll)))

(defn project-as-keys
  ([key-fn coll]
   (project-as-keys {} key-fn coll))
  ([to key-fn coll]
   (project to (fn [x] [(key-fn x) x]) coll)))

(defn ffilter [pred coll]
  (some #(when (pred %) %) coll))

(def update-existing md/update-existing)
(def assoc-some md/assoc-some)

(defn fn-map->transform
  "(fn-map->transform {:number inc}
                   {:number 1})
   => {:number 2}"
  ([fn-map]
   (fn [m]
     (reduce-kv
       (fn [out k f]
         (if-some [v (get m k)]
           (assoc out k (f v))
           out))
       m
       fn-map)
     ))
  ([fn-map m]
   ((fn-map->transform fn-map) m)))

(defn nfurcate
  "(nfurcate {:odd? odd?
           :even? even?} (range 10))
   => {:even? [0 2 4 6 8], :odd? [1 3 5 7 9]}"
  [k->pred coll]
  (reduce
    (fn [out x]
      (let [any-match? (volatile! false)
            out        (reduce-kv (fn [o k pred]
                                    (if (and (pred x) (vreset! any-match? true))
                                      (update o k (comp not-empty vec conj) x)
                                      o))
                                  out
                                  k->pred)]
        (if @any-match?
          (do (vreset! any-match? false)
              out)
          (update out :other (comp not-empty vec conj) x))))
    {}
    coll))

(defn ensure-vec [x]
  (cond
    (vector? x) x
    (nil? x) []
    (sequential? x) (vec x)
    :else [x]))

(defn rcomp
  "Like comp but composes in reverse, so:
  ((comp str inc) 2) => \"3\" becomes
  ((rcomp inc str)) => \"3\""
  {:static true}
  ([] identity)
  ([f] f)
  ([f g]
   (fn
     ([] (g (f)))
     ([x] (g (f x)))
     ([x y] (g (f x y)))
     ([x y z] (g (f x y z)))
     ([x y z & args] (g (apply f x y z args)))))
  ([f g & fs]
   (reduce rcomp (conj fs g f))))

(defmacro f->
  "Wraps clojure.core/-> into an anonymous fn of one arg. Similar to comp, but
   doesn't require forms to be functions."
  [& body]
  `(fn [arg#]
     (-> arg#
         ~@body)))

(defn space-join [& [s & more :as strs]]
  (if (empty? more)
    (str s)
    (str/join " " strs)))

(defn read-edn [s]
  #?(:clj  (read-string s)
     :cljs (cljs.reader/read-string s)))

(defn parse-int [s]
  (when (string? s)
    #?(:clj  (Integer/parseInt s)
       :cljs (js/parseInt s))))

(defn parse-long [s]
  (when (string? s)
    #?(:clj  (Long/parseLong s)
       :cljs (js/parseInt s))))

(defn parse-float [s]
  (when (string? s)
    #?(:clj  (Float/parseFloat s)
       :cljs (js/parseFloat s))))

(def infinity
  #?(:clj  Double/POSITIVE_INFINITY
     :cljs js/Infinity))

(defn debounce [thunk interval]
  #?(:cljs (gfns/debounce thunk interval)
     :clj  (assert false "Not implemented")))

(defn throttle [thunk interval]
  #?(:cljs (gfns/throttle thunk interval)
     :clj  (assert false "Not implemented")))

(defn memoize-ttl [f interval]
  #?(:clj (mem/ttl f :ttl/threshold interval)
     :cljs
          (let [mem (atom {})]
            (fn [& args]
              (if-let [v (get @mem args)]
                v
                (let [ret (apply f args)]
                  (swap! mem assoc args ret)
                  (js/setTimeout
                    #(swap! mem dissoc args)
                    interval)
                  ret))))))

(def memoize-last enc/memoize-last)

(def minute-ms (* 1000 60))
(def hour-ms (* 60 minute-ms))

(defn dedupe-f [f]
  "Like clojure.core/memoize but only caches the last invocation.
  Effectively dedupes invocations with same args."
  (let [cache (atom {})]
    (fn [& args]
      (or (get @cache args)
          (let [ret (apply f args)]
            (reset! cache {args ret})
            ret)))))

(defn keyword-identical? [k1 k2]
  (#?(:clj  identical?
      :cljs cljs/keyword-identical?)
    k1 k2))

(defn f-once [f]
  (let [run? (atom false)]
    (fn once [& args]
      (when-not @run?
        (apply f args)
        (reset! run? true)))))

(defn partition-all-on [n-seq coll]
  (loop [[n & nseq] n-seq out [] coll coll]
    (let [[before after] (split-at n coll)
          out' (conj out before)]
      (if (and nseq (not-empty after))
        (recur nseq out' after)
        out'))))

(def clj? #?(:clj true :cljs false))
(def cljs? #?(:clj false :cljs true))

(defn =by
  ([f]
   (fn [a b]
     (= (f a) (f b))))
  ([f a]
   (let [fa (f a)]
     (fn [b]
       (= fa (f b)))))
  ([f a b]
   (= (f a) (f b)))
  ([f g a b]
   (= (f a) (g b))))

(defn date-instant
  ([]
   #?(:clj  (Date.)
      :cljs (js/Date.)))
  ([ts]
   #?(:clj  (Date. ts)
      :cljs (js/Date. ts))))

(defn date-time []
  (.getTime (date-instant)))

(defn date? [x]
  (instance? #?(:clj  Date
                :cljs js/Date) x))

(defn date-like? [x]
  (or (date? x)
      (t/instant? x)
      (integer? x)))

;; *** DEV & DEBUG

(defn paths-till-pred
  "Given a value-pred and a nested coll, returns a coll
  of paths on which pred returned true:
  (paths-till-pred string? {:a \"foo\"
                            :b [\"foo\" {:c \"fan\"}]})
  ;; => [[:a] [:b 0] [:b 1 :c]]

  Good for _searching_ nested data, reverse engineering API's at the REPL..."
  [v-pred data]
  (let [walker (sp/recursive-path
                 [] p
                 (sp/if-path map?
                             [sp/ALL
                              (sp/if-path [sp/LAST v-pred]
                                          sp/FIRST
                                          [(sp/collect-one sp/FIRST) sp/LAST p])]
                             (sp/if-path sequential?
                                         [sp/INDEXED-VALS
                                          (sp/if-path [sp/LAST v-pred]
                                                      sp/FIRST
                                                      [(sp/collect-one sp/FIRST) sp/LAST p])])))]
    (some->> (sp/select walker data)
             not-empty
             (mapv #(cond-> % (not (vector? %)) vector)))))

(defn tap-xf
  "Debug transducer. taps value
  (optionally with prefix)
   and returns it unchanged."
  ([] (map #(doto % tap>)))
  ([prefix]
   (map #(do (tap> [prefix %]) %))))

(defn prn-xf
  "Debug transducer. Logs message and passes value through."
  ([] (map #(doto % prn)))
  ([prefix]
   (prn-xf prefix {:prn-value? true}))
  ([prefix {:keys [prn-value?]}]
   (map #(do (if prn-value?
               (prn prefix %)
               (prn prefix))
             %))))

(def id-gen
  (nano-id/custom "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" 10))

(defn ensure-id [m]
  (cond-> m (not (:id m)) (assoc :id (id-gen))))

(defn merge-meta
  "Like with-meta but does not override existing meta."
  [x meta-map]
  (with-meta x (into (or (meta x) {}) meta-map)))

(defn deep-merge
  "Merges data-structures recursively. For sequential colls, creates a union
  using the same type as the first data-structure"
  [& [x :as xs]]
  (cond
    (or (sequential? x) (set? x)) (into (empty x) cat xs)
    (map? x) (apply merge-with deep-merge xs)
    :else (last xs)))

(defn template-edn [replace-map expr]
  {:pre [(every? symbol? (keys replace-map))]}
  (walk/postwalk-replace replace-map expr))

(defn wrap
  ([around]
   (fn [s]
     (wrap s around around)))
  ([before after]
   (fn [s]
     (str before s after)))
  ([s before after]
   (str before s after)))

(defn bound [num lower-bound upper-bound]
  (cond
    (<= lower-bound num upper-bound) num
    (< num lower-bound) lower-bound
    (> num upper-bound) upper-bound))

(defn round [points num]
  (let [pts (Math/pow 10 points)]
    (float (/ (Math/round (* pts num)) pts))))

(defn ceil [num]
  (some-> num (Math/ceil) int))

(defn pretty-string [x]
  (when x
    (str/trim (with-out-str (pprint x)))))

(defn prexix-pprint [prefix x]
  (println prefix (pretty-string x)))

(defn delete-directory-recursive
  "Recursively delete a directory."
  [file]
  #?(:clj
     (let [file (cond-> file (string? file) io/file)]
       (when (.exists file)
         (when (.isDirectory file)
           (doseq [file-in-dir (.listFiles file)]
             (delete-directory-recursive file-in-dir)))
         (io/delete-file file)))))

(defn delete-files-recursive
  "Recursively delete a all-files and directories inside a directory."
  [file]
  #?(:clj
     (let [file (cond-> file (string? file) io/file)]
       (when (.exists file)
         (when (.isDirectory file)
           (doseq [file-in-dir (.listFiles file)]
             (delete-directory-recursive file-in-dir)))))))

(defn assoc-missing
  ([m k v]
   (cond-> m
     (false? (contains? m k)) (assoc k v)))
  ([m k v & kvs]
   (let [ret (assoc-missing m k v)]
     (if kvs
       (recur ret (first kvs) (second kvs) (nnext kvs))
       ret))))

(defn partial
  "Works just like clojure.core/partial, but the result can be compared with ="
  [f & args]
  #?(:cljs
     (partial/make-partial-fn f args)))

(defn read-csv [x & {:keys [header->kmap k->cast]}]
  #?(:clj
     (when-let [[headers & rows] (some-> x csv/read-csv)]
       (let [headers (map #(let [h (str/replace % #"^\W*|\W*$" "")]
                             (get header->kmap h h)) headers)
             kcast   (if k->cast
                       (fn-map->transform k->cast)
                       identity)]
         (into []
               (comp
                 (map #(zipmap headers %))
                 (map kcast))
               rows)))))

(defn slurp-csv [x & {:as opts :keys [header->kmap k->cast]}]
  #?(:clj
     (some-> x slurp (read-csv opts))))

(def read-json #?(:clj  json/read-value
                  :cljs js/JSON.parse))

(defn read-json-keywordized [json]
  #?(:clj
     (when json
       (read-json json json/keyword-keys-object-mapper))))

(defn write-json-string [json]
  #?(:clj
     (json/write-value-as-string json json/keyword-keys-object-mapper)))

(defn write-json-bytes [json]
  #?(:clj
     (json/write-value-as-bytes json json/keyword-keys-object-mapper)))

(defn maybe-deref [x]
  #?(:cljs (cond-> x (implements? IDeref x) deref)))

(defn html->hiccup [s]
  (when-let [frags (some-> s hic/parse-fragment not-empty)]
    (case (count frags)
      1 (hic/as-hiccup (first frags))
      (into [:div {}] (map hic/as-hiccup) frags))))

#?(:clj
   (defn- clipboard []
     (.getSystemClipboard (java.awt.Toolkit/getDefaultToolkit))))

#?(:clj
   (defn cb
     "Clipboard util
     (cb) returns current clipboard data
     (cb 123) puts 123 into clipboard
     (cb true 123) makes it pretty and puts it into clipboard"
     ([]
      (try
        (.getTransferData (.getContents (clipboard) nil) (java.awt.datatransfer.DataFlavor/stringFlavor))
        (catch java.lang.NullPointerException e nil)))
     ([x] (cb true x))
     ([pretty? x]
      (let [text      (if (string? x)
                        x
                        (if pretty?
                          (with-out-str (clojure.pprint/pprint x))
                          (pr-str x)))
            selection (java.awt.datatransfer.StringSelection. text)]
        (.setContents (clipboard) selection selection))
      x)))