(ns stuffs.util
  (:require [clojure.string :as str]
            [com.rpl.specter :as sp]
            [nano-id.core :as nano-id]
            [clojure.walk :as walk]
            [clojure.pprint :refer [pprint print-table]]
            [clojure.set :as set]
            [hickory.core :as hic]
            [stuffs.js-interop :as j]
            [taoensso.encore :as enc]
            [stuffs.env :as env]
            [lambdaisland.regal :as regal]
            [net.cgrand.xforms :as x :include-macros true]
            [net.cgrand.xforms.rfs :as rf :include-macros true]
            #?@(:clj  [[clojure.java.io :as io]
                       [uix.dom.alpha :as uix.dom]
                       [clojure.data.csv :as csv]
                       [jsonista.core :as json]
                       [clojure.core.memoize :as mem]
                       [stuffs.impl.string :as sstr]]
                :cljs [[goog.functions :as gfns]
                       [goog.string :as gstr]
                       [cljs.core :as cljs]
                       [stuffs.impl.partial :as partial]
                       [tick.locale-en-us]])
            [medley.core :as md]
            [tick.core :as t])
  (:refer-clojure :exclude [#?(:cljs keyword-identical?) parse-long partial])
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

(defmacro namespace-str []
  (or (some-> (:ns &env) :name str)                         ; cljs
      `(str *ns*)))                                         ; clj

(declare contains-white-space?)

(defn ->kw-str [string-kw-or-sym]
  (let [s (some-> string-kw-or-sym name str/trim not-empty)]
    (if (and s (not (contains-white-space? s)))
      s
      (throw (ex-info (str "Invalid input: " string-kw-or-sym)
                      {:input string-kw-or-sym})))))

(defn str-starts-with-number? [s]
  (and (string? s)
       (boolean (re-find #"^\d" s))))

(def namespace-key
  (letfn [(throw-when-str-starts-with-number [s]
            (if (str-starts-with-number? s)
              (throw (ex-info (str "key cannot start with number: " s)
                              {:k s}))
              s))]
    (fn ns-k
      ([nsp]
       (let [nsp' (->kw-str nsp)]
         (fn nsk [k]
           (keyword nsp' (throw-when-str-starts-with-number (->kw-str k))))))
      ([nsp k]
       (keyword (->kw-str nsp) (throw-when-str-starts-with-number (->kw-str k)))))))

(defn un-ns-k [k]
  (some-> k name keyword))

(defn k->ns-k [k]
  (some-> k namespace keyword))

(defn prefix-key [prefix k]
  (keyword (str (name prefix) (name k))))

(defn prefix-keys [prefix m]
  (project
    (fn [[k v]]
      [(prefix-key prefix k) v])
    m))

(defn namespace-keys [prefix m]
  (let [prefix (str prefix "/")]
    (prefix-keys prefix m)))

(defn ffilter [pred coll]
  (some #(when (pred %) %) coll))

(defn fn-map
  "(fn-map {:odd? odd? :even? even?} 1)
   => {:odd? true, :even? false}"
  ([f-map]
   (fn [x]
     (reduce-kv
       (fn [out k f]
         (assoc out k (f x)))
       {}
       f-map)))
  ([f-map x]
   ((fn-map f-map) x)))

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

(defn xf-map-reduce
  "(xf-map-reduce
  {:cost (x/reduce +)
   :pct  x/avg}
  [{:cost 1
    :pct  0.8}
   {:cost 1
    :pct  0.7}])
  => {:cost 2, :pct 0.75}"
  ([xfm]
   (x/transjuxt
     (into {}
           (map (fn [[k xf]]
                  [k (comp (map k)
                           xf)]))
           xfm)))
  ([xfm coll]
   (transduce (xf-map-reduce xfm) rf/last coll)))

(defn xf->step
  "Single input transducer execution, e.g.
  (xf->step (map inc) 1) => 2"
  ([xform]
   (fn [x]
     (xf->step xform x)))
  ([xform x]
   ((xform (fn [_ result] result)) nil x)))

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

(defn ensure-set [x]
  (cond
    (set? x) x
    (nil? x) #{}
    (sequential? x) (set x)
    :else #{x}))

(defn remove-nil-vals [m]
  (md/remove-vals nil? m))

(def dissoc-in md/dissoc-in)
(def assoc-some md/assoc-some)
(def update-existing md/update-existing)

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

(defn some-comp
  "Takes a set of functions and returns a fn that is the composition
  of those fns.  The returned fn takes a variable number of args,
  applies the rightmost of fns to the args, the next
  fn (right-to-left) to the result, etc."
  {:added  "1.0"
   :static true}
  ([] identity)
  ([f] f)
  ([f g]
   (fn
     ([] (some-> (g) f))
     ([x] (some-> (g x) f))
     ([x y] (some-> (g x y) f))
     ([x y z] (some-> (g x y z) f))
     ([x y z & args] (some-> (apply g x y z args) f))))
  ([f g & fs]
   (reduce some-comp (list* f g fs))))

(defmacro f->
  "Wraps clojure.core/-> into an anonymous fn of one arg. Similar to comp, but
   doesn't require forms to be functions."
  [& body]
  `(fn [arg#]
     (-> arg#
         ~@body)))

(defn some->m [m & ks-fns]
  (assert (even? (count ks-fns)))
  (reduce (fn [m [k f]]
            (if-some [ret (f m)]
              (assoc m k ret)
              (reduced m)))
          m
          (partition 2 ks-fns)))

(defmacro <-
  "Converts a ->> to a ->
   (->> (range 10) (map inc) (<- (doto prn)) (reduce +))"
  [& body]
  `(-> ~(last body) ~@(butlast body)))

#_(defn space-join [& [s & more :as strs]]
    (if (empty? more)
      (str s)
      (str/join " " strs)))

(defmacro sep-join
  "Like (apply core.string/join \" \" coll) but runs at compile time using str."
  [sep & xs]
  `(str ~@(rest (interleave (repeat sep) xs))))

(defmacro space-join
  "Like (apply core.string/join \" \" coll) but runs at compile time using str."
  [& xs]
  `(sep-join " " ~@(filter identity xs)))

(defn numbered-join
  "((numbered-join {:num-right-s \".\" :sep \"\\n\"}) [\"foo\" \"bar\"])\n=> \"1. foo\\n2. bar\""
  [{:keys [sep num-left-s num-right-s coll start-n]
    :or   {sep         " "
           num-right-s "."
           start-n     1}}]
  (letfn [(str-numbered-join [coll]
            (x/str
              (comp (map-indexed (fn [i s]
                                   (str num-left-s (+ i start-n) num-right-s " " s)))
                    (interpose sep))
              coll))]
    (if coll
      (str-numbered-join coll)
      str-numbered-join)))

(defn str-comma-and-join [coll]
  (case (count coll)
    0 ""
    1 (first coll)
    2 (str (first coll) " and " (second coll))
    (str (str/join ", " (butlast coll)) " and " (last coll))))

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
     :clj  (println `debounce "Not implemented")))

(defn throttle [thunk interval]
  #?(:cljs (gfns/throttle thunk interval)
     :clj  (println `debounce "Not implemented")))

(defn memoize-ttl [f interval]
  #?(:clj  (mem/ttl f :ttl/threshold interval)
     :cljs (let [mem (atom {})]
             (fn [& args]
               (if-let [v (get @mem args)]
                 v
                 (let [ret (apply f args)]
                   (swap! mem assoc args ret)
                   ;; FIXME this will not cancel a dissoc on a next invocation
                   (js/setTimeout
                     #(swap! mem dissoc args)
                     interval)
                   ret))))))

(def memoize-last enc/memoize-last)

(def minute-ms (* 1000 60))
(def hour-ms (* 60 minute-ms))

(defn memoize-ttl-4h [f]
  (memoize-ttl f (* 4 hour-ms)))

(defn dedupe-f [f]
  "Like clojure.core/memoize but only caches the last invocation.
  Effectively dedupes invocations with same args."
  (let [cache (atom {})]
    (fn [& args]
      (if-let [[_ v] (find @cache args)]
        v
        (let [ret (apply f args)]
          (reset! cache {args ret})
          ret)))))

(defn keyword-identical? [k1 k2]
  (#?(:clj  identical?
      :cljs cljs/keyword-identical?)
    k1 k2))

(defn f-once
  "Runs f one time only"
  [f]
  (let [run? (atom false)]
    (fn once [& args]
      (when-not @run?
        (apply f args)
        (reset! run? true)))))

(defn f-skip
  "skip's n executions of f"
  [n f]
  {:pre [(pos-int? n)]}
  (let [!n (atom n)]
    (fn skip [& args]
      (if (zero? @!n)
        (apply f args)
        (swap! !n dec)))))

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

(defn date-time
  ([]
   (.getTime (date-instant)))
  ([d] (.getTime d)))

(defn date? [x]
  (instance? #?(:clj  Date
                :cljs js/Date) x))

(defn date-like? [x]
  (or (date? x)
      (t/instant? x)
      (integer? x)))

(def local-date-time-string
  (let [formatter (t/formatter "uuuu/MM/dd hh:mm a")]
    (fn
      ([]
       (t/format formatter (t/zoned-date-time)))
      ([v]
       (t/format formatter (t/in v (t/zone)))))))

(defn current-time-zone []
  #?(:cljs (-> (j/call js/Intl :DateTimeFormat)
               (j/call :resolvedOptions)
               (j/get :timeZone))
     :clj  (str (t/zone))))

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

(defmacro dtap
  "Like tap> but only tabs in dev and returns value instead of true"
  [x]
  `(do
     ~(when env/dev?
        `(tap> ~x))
     ~x))

(defn identity-thunk [thunk]
  (thunk))

(def identity-xf (map identity))

(defn doto-xf [f]
  (map #(doto % f)))

(defn tap-xf
  "Debug transducer. taps value
  (optionally with prefix)
   and returns it unchanged."
  ([] (doto-xf tap>))
  ([prefix]
   (doto-xf #(tap> [prefix %]))))

(defn prn-xf
  "Debug transducer. Logs message and passes value through."
  ([] (doto-xf prn))
  ([prefix]
   (prn-xf prefix {}))
  ([prefix {:keys [f prn-value? pred]
            :or   {prn-value? true
                   f          identity
                   pred       (constantly true)}}]
   (map
     #(do
        (when (pred %)
          (if prn-value?
            (prn prefix (f %))
            (prn prefix)))
        %))))

(defn pred-xf [pred f]
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result input]
       (rf result (cond-> input (pred input) f))))))

(defn cond->xf [test xf]
  (if test
    xf
    identity-xf))

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

(defn normalize
  ([nums]
   (let [mi (apply min nums)
         mx (apply max nums)
         f  (normalize mi mx)]
     (map f nums)))
  ([min max]
   {:pre (> max min)}
   (let [max' (- max min)]
     (fn [num]
       (let [normd (/ (- num min) max')]
         #_(println (float normd))
         (assert (>= 1 normd 0))
         normd)))))


(defn round [points num]
  (let [pts (Math/pow 10 points)]
    (float (/ (Math/round (* pts num)) pts))))

(defn round-2
  "Rounds to two decimals"
  [num]
  (round 2 num))

(defn ceil [num]
  (some-> num (Math/ceil) int))

(defn floor [num]
  (some-> num (Math/floor) int))

(defn pp [& xs]
  (when (not-empty xs)
    (run! pprint xs)))

(def ppt print-table)

(defn pretty-string [x]
  (when x
    (str/trim (with-out-str (pprint x)))))

(defn prefix-pprint [prefix x]
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
       (let [headers (map #(let [h (str/trim %)]
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

(defn write-csv-string [x & {:keys [k->headermap]}]
  #?(:clj
     (let [ks      (if k->headermap
                     (keys k->headermap)
                     (keys (first x)))
           headers (-> (if k->headermap
                         (vals k->headermap)
                         (map name ks))
                       not-empty
                       vec)]
       (with-out-str
         *out*
         (csv/write-csv
           *out*
           (into [headers]
                 (map (fn [x] (map #(get x %) ks)))
                 x))))))

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

(defn hiccup->html [h]
  #?(:clj
     (uix.dom/render-to-static-markup h)))

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
        (.setContents (clipboard) selection nil))
      x)))

(defn simple-snake->camel-case [s]
  (str/replace s #"_" "-"))

(defn simple->camel-case [s]
  (str/replace (str/lower-case s) #"\s+" "-"))

(defn space->dash [s]
  (-> s
      str/trim
      (str/replace #"\s+" "-")))

(def contains-white-space?
  (let [r (regal/regex [:cat :whitespace])]
    (fn [s]
      (boolean (re-find r s)))))

(def remove-parens-and-content
  (let [r (regal/regex [:cat [:* " "] "(" [:* [:not ")"]] ")" [:* " "]])]
    (fn [s]
      (-> s
          (str/replace r " ")
          str/trim))))

(def collapse-whitespace
  ;; whitespace regex copied from `gstr/collapseWhitespace` impl
  #?(:clj  (comp str/trim #(str/replace % #"[\s\xa0]+" " "))
     :cljs gstr/collapseWhitespace))

;; https://github.com/brandonbloom/fipp/issues/77
(def url-regex
  (re-pattern "^https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b([-a-zA-Z0-9()@:%_\\+.~#?&//=]*)$"))

(defn url? [x]
  (boolean
    (and (string? x)
         (re-find url-regex x))))

(defn url-remove-http-www-q-params [x]
  (and (url? x)
       (str/replace x (re-pattern "http(s)?(:)?(\\/\\/)?|(\\/\\/)?(www\\.)?(/?\\?.*)?(\\/$)?") "")))

(comment
  (require '[lambdaisland.regal.parse :as rp])
  (remove-parens-and-content "askjasdlas")
  (rp/parse "(?<=\\/clerk\\/admin\\/).*")
  (re-find
    (regal/regex
      (rp/parse #"(?<=\/clerk\/admin\/).*"))
    "/clerk/admin/_ws")
  (re-find #"(?<=\/clerk\/admin\/).*"
           "/clerk/admin/_ws/foo")
  )

(defn strl
  "same as (str/lower-case (str x))"
  [x]
  (-> x str str/lower-case))

(defmacro str-interpolate [& strings]
  `(sstr/interpolate ~@strings))

(def str-unquote
  (let [start-end-quotes (delay (regal/regex [:alt [:cat :start "\""] [:cat "\"" :end]]))]
    (fn [s]
      (-> s
          str/trim
          (str/replace @start-end-quotes "")))))

(defn str-ends-with-whitespace? [s]
  (boolean (re-find #"\s$" s)))

(defmacro ascertain
  ([expr]
   (list `ascertain expr "could not ascertain"))
  ([expr message]
   `(or
      ~expr
      (throw (ex-info (str "Could not assert: " (pr-str '~expr) "\n\n" ~message) {})))))

(defmacro ascertain-some
  ([expr]
   (list `ascertain-some expr "could not ascertain"))
  ([expr message]
   `(if-some [ret# ~expr]
      ret#
      (throw (ex-info (str ~message " " (pr-str '~expr)) {})))))

#_(defn eng-postfix-fmt-num [n]
    (let [sn       (str n)
          post-fix (case sn
                     ("11" "12" "13") "th"
                     (condp (fn [num sn]
                              (str/ends-with? sn num))
                            sn
                       "1" "st"
                       "2" "nd"
                       "3" "rd"
                       "th"))]
      (str sn post-fix)))

#_(comment
    (map #(eng-postfix-fmt-num %) (range 1 35))
    )

(defmacro when-f
  "Like when for function composition.
   Instead of returning nil a falsy test returns identity.
  ((when-f false inc) 1)
  (when-f env/dev? #(doto % tap>))"
  [test body-f]
  (list 'if test body-f identity))

(defn pred-f
  "((pred-f number? str) 123)
=> \"123\"
((pred-f number? str) :not-this)
=> :not-this"
  [pred f]
  (fn when-pred-f [x]
    (cond-> x
      (pred x) f)))