(ns stuffs.css)

(def spacing
  "Spacing that matches Tachyons
  https://github.com/tachyons-css/tachyons-spacing"
  [0 0.25 0.5 1 2 4 8 16])

(defn style [[selector decl-map]]
  (str (name selector)
       " { "
       (transduce
         (comp (map (fn [[k v]]
                      (str (name k) ": " (name v))))
               (interpose "; "))
         str
         decl-map)
       " }"))

(defn styles [styles]
  (transduce (map style) str styles))

(defn spread-style-xf [sel decl-k unit]
   (map-indexed (fn [i v]
                  (style [(str (name sel) i)
                          {decl-k (str v (name unit))}]))))
(def spaced-gap
  (transduce (spread-style-xf :.gap :gap :rem) str spacing))

(def max-height
  (transduce (spread-style-xf :.mxh :max-height :rem) str spacing))
