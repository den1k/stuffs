(ns stuffs.hiccup)

(defn document
  [{:as opts :keys [js styles links meta body title]}]
  [:html
   [:head
    (for [m meta]
      [:meta m])
    (for [s styles]
      [:style {:type                    "text/css"
               :dangerouslySetInnerHTML {:__html s}}])
    (for [l links]
      [:link {:rel "stylesheet" :href l}])
    (when title [:title title])]
   [:body
    body
    (for [{:keys [src script]} js]
      [:script
       (if src
         {:src src}
         {:dangerouslySetInnerHTML {:__html script}})])]])

(defn html [{:as opts :keys [js styles links meta body]}]
  [:<>
   [:meta {:charset "UTF-8"}]
   [document opts]])
