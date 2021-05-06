(ns stuffs.dom
  (:require [clojure.string :as str]
            [kitchen-async.promise :as p]
            [stuffs.js-interop :as j]
            #?@(:cljs [[goog.object :as gobj]]
                :clj  [[net.cgrand.macrovich :as macros]])
            [net.cgrand.xforms :as x]
            [stuffs.util :as u])
  #?(:cljs (:require-macros [stuffs.dom])))

(defn mobile-device? []
  #?(:cljs
     (let [user-agent (str/lower-case (j/get-in js/window [:navigator :userAgent]))
           rxp        #"android|webos|iphone|ipad|ipod|blackberry|iemobile|opera mini"]
       (boolean (re-find rxp user-agent)))))

(defn copy-to-clipboard
  ([clipboard-text-or-fn] (copy-to-clipboard clipboard-text-or-fn (constantly nil)))
  ([clipboard-text-or-fn on-copied]
   #?(:cljs
      (p/then
        (if (fn? clipboard-text-or-fn)
          (clipboard-text-or-fn)
          clipboard-text-or-fn)
        (fn [txt]
          (p/then (j/call-in js/navigator [:clipboard :writeText] txt)
                  on-copied))))))

(defn read-clipboard-promise []
  #?(:cljs
     (j/call-in js/navigator [:clipboard :readText])))

(defn read-clipboard [on-content]
  #?(:cljs
     (p/then (read-clipboard-promise) on-content)))

(defn bounding-rect [node]
  #?(:cljs
     (j/lookup (.getBoundingClientRect node))))

(defn sel-rect
  ([]
   #?(:cljs (sel-rect (j/call js/document :getSelection))))
  ([sel]
   (let [sel-range (.getRangeAt sel 0)
         {:as r :keys [x y]} (bounding-rect sel-range)]
     (if-not (zero? y)
       r
       (bounding-rect (.-anchorNode sel))))))

; https://developer.mozilla.org/en-US/docs/Web/API/TreeWalker

(defn text-walker
  ([root-node]
   #?(:cljs
      (.createTreeWalker
        js/document
        root-node js/NodeFilter.SHOW_TEXT)))
  ([root-node current-node]
   #?(:cljs
      (j/assoc! (text-walker root-node) :currentNode current-node))))

(defn clone-walker [walker]
  #?(:cljs
     (-> (j/call
           js/document
           :createTreeWalker
           (j/get walker :root)
           (j/get walker :whatToShow)
           (j/get walker :filter))
         (j/assoc! :currentNode (j/get walker :currentNode)))))

(defn walker-all-nodes [walker]
  #?(:cljs
     (sequence (take-while some?)
               (repeatedly #(j/call walker :nextNode)))))

(defn walker-take-while [walker pred]
  #?(:cljs
     (sequence
       (comp (take-while some?)
             (take-while pred))
       (repeatedly #(j/call walker :nextNode)))))

(defn ensure-element-node
  "Ensures HTML element node since some lookup functions are not available elsewhere.
  https://developer.mozilla.org/en-US/docs/Web/API/Node/nodeType"
  [node]
  #?(:cljs
     (when node
       (if (not= 1 (j/get node :nodeType))
         (ensure-element-node (j/get node :parentNode))
         node))))

(defn deepest-first-child [node]
  #?(:cljs
     (if-let [ch (j/get node :firstChild)]
       (deepest-first-child ch)
       node)))

(defn deepest-last-child [node]
  #?(:cljs
     (if-let [ch (j/get node :lastChild)]
       (deepest-last-child ch)
       node)))

(defn tag-name [node]
  #?(:cljs
     (some-> node (j/get :tagName) str/lower-case keyword)))

(defn closest-node
  ([selector]
   #?(:cljs
      (fn closest [node]
        (some-> (ensure-element-node node)
                (j/call :closest selector)))))
  ([node selector]
   #?(:cljs
      (some-> (ensure-element-node node)
              (j/call :closest selector)))))

(def line-regex #"\n|\r\n")

(defn line-count [s]
  ;; all linesbreaks + first line
  (inc (count (re-seq line-regex s))))

(def zero-width-character
  ;"\u8710" ; debug
  "\u200B")

(defn remove-zero-width-character [s]
  (str/replace s zero-width-character ""))

(defn get-selection* []
  #?(:cljs
     (let [sel         (j/call js/document :getSelection)
           anchor-node (j/get sel :anchorNode)]
       (when anchor-node
         (let [sel-range     (j/call sel :getRangeAt 0)
               start         (j/get sel-range :startOffset)
               end           (j/get sel-range :endOffset)
               anchor-string (j/get anchor-node :textContent)]
           {:sel          sel
            :sel-range    sel-range
            :range?       (not (j/get sel :isCollapsed))
            :anchor-node  anchor-node
            :anchor-start start
            :anchor-end   end
            :anchor-str   anchor-string
            :anchor-strl  (subs anchor-string 0 start)
            :anchor-strr  (subs anchor-string end)})))))


(defn get-attribute [node attr-str]
  #?(:cljs
     (some-> node
             ensure-element-node
             (j/call :getAttribute (name attr-str)))))

(defn attr-val-query-selector [attr val]
  #?(:cljs
     (j/call js/document :querySelector (str "[" attr "=\"" val "\"]"))))


(defn sel->xy [{:keys [sel-rect]}]
  {:x (:x sel-rect)
   :y (+ (:height sel-rect) (:y sel-rect))})

(defn sel-xy []
  (sel->xy {:sel-rect (sel-rect)}))

(defn relative-rect
  [rect-a rect-b]
  {:top    (- (:top rect-b) (:top rect-a))
   :right  (- (:right rect-b) (:right rect-a))
   :bottom (- (:bottom rect-b) (:bottom rect-a))
   :left   (- (:left rect-b) (:left rect-a))
   :x      (- (:x rect-b) (:x rect-a))
   :y      (- (:y rect-b) (:y rect-a))
   :width  (:width rect-b)
   :height (:height rect-b)})

(defn get-computed-style-property-value [node property-key]
  #?(:cljs
     (some-> (j/call js/window :getComputedStyle node)
             (j/call :getPropertyValue property-key))))

(defn set-selection* [start-node start-offset end-node end-offset]
  #?(:cljs
     (when (and start-node end-node)
       (let [range (doto (j/call js/document :createRange)
                     (j/call :setStart start-node start-offset)
                     (j/call :setEnd end-node end-offset))]
         (doto (j/call js/window :getSelection)
           (j/call :removeAllRanges)
           (j/call :addRange range))))))

(defn set-selection [node start end]
  #?(:cljs
     (let [range (doto (.createRange js/document)
                   (.setStart node start)
                   (.setEnd node end))]
       (doto (.getSelection js/window)
         .removeAllRanges
         (.addRange range)))))

(defn text-length-before [parent-node anchor-node]
  (let [walker (text-walker parent-node anchor-node)]
    (transduce (take-while some?)
               (completing
                 (fn sum-length [acc input]
                   (+ acc (.-length input))))
               0
               (repeatedly #(.previousNode walker)))))

(defn -node-at-index [parent-node idx]
  (let [walker (text-walker parent-node)]
    (transduce (take-while some?)
               (completing
                 (fn sum-length [acc input]
                   (let [len   (.-length input)
                         total (+ acc len)]
                     (if (>= total idx)
                       (reduced {:node input :offset (- len (- total idx))})
                       total)))
                 (fn [ret]
                   (when (map? ret) ret)))
               0
               (repeatedly #(.nextNode walker)))))

(defn set-selection-first-child [node start end]
  #?(:cljs
     (let [fchild (.-firstChild node)
           range  (doto (.createRange js/document)
                    (.setStart fchild start)
                    (.setEnd fchild end))]
       (doto (.getSelection js/window)
         .removeAllRanges
         (.addRange range)))))

(defn- text-content-length [node]
  (.-length (.-textContent node)))

(defn set-caret
  "Sets the caret to a certain idx in a node.
  If the idx is zero and the node is empty it
  must have the following style set on it:
  .node-class:empty:not(:focus)::before {
      content: attr(placeholder);
      color: graytext;
      cursor: text;
    }"
  [node idx]
  #?(:cljs
     (set-selection node idx idx)))

(defn set-caret-absolute [node idx]
  (when-let [{:keys [node offset]} (-node-at-index node idx)]
    (set-caret node offset)))

(defn set-selection-absolute [node start end]
  #?(:cljs
     (let [{start-node :node start-offset :offset} (-node-at-index node start)
           {end-node :node end-offset :offset} (-node-at-index node end)]
       (set-selection* start-node start-offset end-node end-offset))))

(defn delete-selection []
  #?(:cljs
     (some-> (j/call js/document :getSelection)
             (j/call :deleteFromDocument))))

(defn set-caret-to-end
  "Sets the caret to the end of a node"
  [node]
  (let [child (deepest-last-child node)]
    (set-selection child (text-content-length child) (text-content-length child))))

(defn set-caret-first-child [node idx]
  #?(:cljs
     (set-selection-first-child node idx idx)))

(defn replace-selection [text]
  #?(:cljs
     (when-let [{:keys [anchor-start anchor-end anchor-node]} (get-selection*)]
       (if (= anchor-start anchor-end)
         (let [latter-node (.splitText anchor-node anchor-start)
               txt-node    (j/call js/document :createTextNode text)]
           (j/call latter-node :before txt-node)
           (set-caret-to-end txt-node))
         (js/console.log "TODO implement this")))))

(defn wrap-selection
  ([wrap] (wrap-selection wrap wrap))
  ([before after]
   #?(:cljs
      (when-let [{:keys [sel-range]} (get-selection*)]
        (let [span (j/call js/document :createElement "span")]
          (doto span
            (j/call :append (j/call js/document :createTextNode before))
            (j/call :appendChild (j/call sel-range :extractContents))
            (j/call :append (j/call js/document :createTextNode after)))
          (j/call sel-range :insertNode span))))))

#?(:clj
   (defmacro e->
     "Event handler macro. Wraps clojure.core/-> into an fn that takes e and
     threads it through exprs. Similar to comp, but doesn't require forms to be
     functions."
     [& body]
     `(fn e-># [e#]
        (-> e#
            ~@body))))

#?(:clj
   (defmacro e->>
     "Event handler macro. Wraps clojure.core/->> into an fn that takes e and
     threads it through exprs. Similar to comp, but doesn't require forms to be
     functions."
     [& body]
     `(fn e->># [e#]
        (->> e#
             ~@body))))

#?(:clj
   (defmacro e>
     "Event handler macro. Makes e (event) and target available in the body."
     ([& body]
      (macros/case
        :cljs
        `(fn e># [~'e]
           (let [~'target (.-target ~'e)
                 ~'value (.-value ~'target)
                 ~'inner-html (.-innerHTML ~'target)
                 ~'inner-text (.-innerText ~'target)
                 ~'text-content (.-textContent ~'target)
                 ~'event-vals {:e            ~'e
                               :target       ~'target
                               :value        ~'value
                               :inner-html   ~'inner-html
                               :inner-text   ~'inner-text
                               :text-content ~'text-content}]
             ~@body))))))

(defn- fn-or-multifn? [x]
  #?(:cljs
     (or (fn? x)
         (identical? (type x) MultiFn))))

#?(:cljs
   (defn hiccup->dom
     ([x]
      (hiccup->dom {} x))
     ([{:as ctx :keys [upstream-svg?]} x]
      (cond
        (nil? x) x
        (string? x) x
        ;; html elems
        (j/get x :nodeType) x
        (vector? x)
        (let [[tag attrs & children] x]
          ;#spy/c [:x (fn-or-multifn? tag) x]
          (if (fn-or-multifn? tag)
            (hiccup->dom ctx (apply tag attrs children))
            (let [svg?        (u/keyword-identical? :svg tag)
                  el          (if (or svg? upstream-svg?)
                                (j/call js/document
                                        :createElementNS
                                        "http://www.w3.org/2000/svg"
                                        (name tag))
                                (j/call js/document
                                        :createElement
                                        (name tag)))
                  set-attr-fn (if svg?
                                (fn [k v]
                                  (.setAttributeNS el nil k v))
                                (fn [k v]
                                  (j/call el :setAttribute k v)))]
              (doseq [[k v] attrs
                      :when (some? v)]
                (case k
                  :ref (v el)
                  :on-click (j/call el :addEventListener "click" v)
                  (set-attr-fn (name k)
                               (cond
                                 (string? v) v
                                 (true? v) ""
                                 (false? v) "false"
                                 (fn? v) ""                 ; ignore for now
                                 (keyword? v) (name v)
                                 (map? v) (transduce
                                            (map (fn [[k v]]
                                                   (str (name k) ": " v ";")))
                                            str
                                            v)
                                 (vector? v) (transduce (comp
                                                          (keep identity)
                                                          (map name)) (completing #(str % " " %2))
                                                        v)))))
              (when (not-empty children)
                (j/apply el :append (transduce (keep #(hiccup->dom
                                                        (cond-> ctx
                                                          svg? (assoc :upstream-svg? svg?))
                                                        %))
                                               (completing
                                                 (fn [out i]
                                                   (j/call out :push i)
                                                   out))
                                               #js []
                                               children)))
              #_(when-let [{:keys [data-id]} attrs]
                  (swap! id->node assoc data-id el)
                  )
              el)))))))

(defn extract-right! [{:keys [root-elem start-node start-offset]}]
  #?(:cljs
     (let [r (doto (j/call js/document :createRange)
               (j/call :setStart start-node start-offset)
               (j/call :setEndAfter (deepest-last-child root-elem)))]
       (j/call r :extractContents))))

(defn clone-right! [{:as opts :keys [root-elem start-node start-offset]}]
  #?(:cljs
     (let [right-elems (extract-right! opts)
           tag         (tag-name root-elem)
           clone       (hiccup->dom [tag {} right-elems])]
       clone)))

(defn cursor-beginning? [rel-node]
  #?(:cljs
     (let [sel (j/call js/document :getSelection)]
       (and (j/get sel :isCollapsed)
            (zero? (j/get sel :anchorOffset))
            (= (deepest-first-child rel-node)
               (j/get sel :anchorNode))))))

(defn delete-backwards! [node offset num-chars]
  "https://developer.mozilla.org/en-US/docs/Web/API/Node/nodeValue"
  #?(:cljs
     (let [del-offset (- offset num-chars)]
       ;; this will fail when del-offset extends beyon the start of the element
       ;; in that case a range should be made and contents deleted from the dom
       (j/update! node :nodeValue
                  (fn [s]
                    (str (subs s 0 del-offset)
                         (subs s offset))))
       del-offset)))

(defn scroll-to-bottom [node]
  #?(:cljs
     (j/assoc! node :scrollTop (j/get node :scrollHeight))))

(defn scroll-into-view [node & [opts]]
  #?(:cljs (some-> node (.scrollIntoView (some-> opts clj->js)))))

(defn resize-to-scoll-height [node]
  (some-> node
          (j/assoc-in! [:style :height] "auto")
          (j/assoc-in! [:style :height]
                       (j/get node :scrollHeight))))

(defn resize-to-scoll-width [node]
  (some-> node
          (j/assoc-in! [:style :width] "auto")
          (j/assoc-in! [:style :width]
                       (j/get node :scrollWidth))))

(defn resize-to-scroll-dims [node]
  (doto node
    resize-to-scoll-width
    resize-to-scoll-height))

(defn node-rendered? [node]
  #?(:cljs
     (j/call-in js/document [:body :contains] node)))

(defn on-focus [cb]
  #?(:cljs
     (.addEventListener js/document
                        "focus"
                        cb
                        ; capture
                        true)))

(defn js-fetch [url opts]
  #?(:cljs
     (js/fetch url (clj->js opts))))

;; DataTransfer

(defn data-transfer-with-files? [dt]
  (pos? (j/get-in dt [:files :length])))

(defn data-transfer->form-data [dt]
  #?(:cljs
     (when-let [files (j/get dt :files)]
       (let [form-data (js/FormData.)]
         (run! #(.append form-data (str (gensym)) % (j/get % :name)) files)
         form-data))))

(defn data-transfer-type+data [dt]
  (cond
    (data-transfer-with-files? dt) [:files (data-transfer->form-data dt)]
    :else (when-let [t (j/call dt :getData "text")] [:text t])))

(defonce listeners (atom {}))

(defn add-listener
  ([k type handler]
   #?(:cljs
      (add-listener k type js/document handler)))
  ([k type node handler]
   (swap! listeners update k
          (fn [[cur-node cur-handler]]
            (when (node-rendered? cur-node)
              (j/call cur-node :removeEventListener cur-handler))
            (j/call node :addEventListener type handler)
            [node handler]))))