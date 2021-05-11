(ns stuffs.keybind
  "CLJC version of https://github.com/piranha/keybind/blob/master/src/keybind/core.cljs"
  (:require [clojure.string :as str]
            [stuffs.util :as u])
  #?(:cljs (:require-macros [stuffs.keybind])))

;; Definitions
(def MODS
  {"shift"  :shift
   "ctrl"   :ctrl "control" :ctrl "C" :ctrl
   "alt"    :alt "option" :alt "M" :alt
   "win"    :meta "cmd" :meta "super" :meta "meta" :meta "S" :meta
   ;; default modifier for OS X is cmd and for others is ctrl
   "defmod" (if #?(:cljs (neg? (.indexOf js/navigator.userAgent "Mac OS X"))
                   :clj  false)
              :ctrl
              :meta)})

(defn mod-key? [e]
  (boolean
    (or (.-shiftKey e)
        (.-ctrlKey e)
        (.-altKey e)
        (.-metaKey e))))

(def KEYATTRS
  {:shift "shiftKey" :ctrl "ctrlKey" :alt "altKey" :meta "metaKey"
   :code  "keyCode"})

(def DEFCHORD {:shift false :ctrl false :alt false :meta false})

(def num-pad
  ;(into (sorted-map) (for [i (range 10)] [(str "num-" i) (+ 95 i)]))
  {"num-0" 95
   "num-1" 96
   "num-2" 97
   "num-3" 98
   "num-4" 99
   "num-5" 100
   "num-6" 101
   "num-7" 102
   "num-8" 103
   "num-9" 104})

(def top-row
  ;(into (sorted-map) (for [i (range 10)]
  ;                     [(str i) (+ 48 i)]))
  {"0" 48 "1" 49 "2" 50 "3" 51 "4" 52 "5" 53 "6" 54 "7" 55 "8" 56 "9" 57})

(def f1-f24
  ;(into (sorted-map) (for [i (range 1 25)]
  ;                     [(str "f" i) (+ 111 i)]))
  {"f1"  112
   "f10" 121
   "f11" 122
   "f12" 123
   "f13" 124
   "f14" 125
   "f15" 126
   "f16" 127
   "f17" 128
   "f18" 129
   "f19" 130
   "f2"  113
   "f20" 131
   "f21" 132
   "f22" 133
   "f23" 134
   "f24" 135
   "f3"  114
   "f4"  115
   "f5"  116
   "f6"  117
   "f7"  118
   "f8"  119
   "f9"  120})

(def alphabet
  ;(into (sorted-map) (for [i (range 65 91)]
  ;                     [(.toLowerCase (js/String.fromCharCode i)) i]))
  {"a" 65
   "b" 66
   "c" 67
   "d" 68
   "e" 69
   "f" 70
   "g" 71
   "h" 72
   "i" 73
   "j" 74
   "k" 75
   "l" 76
   "m" 77
   "n" 78
   "o" 79
   "p" 80
   "q" 81
   "r" 82
   "s" 83
   "t" 84
   "u" 85
   "v" 86
   "w" 87
   "x" 88
   "y" 89
   "z" 90})

(def KEYS
  (merge {"backspace" 8
          "tab"       9
          "enter"     13 "return" 13
          "pause"     19
          "caps"      20 "capslock" 20
          "escape"    27 "esc" 27
          "space"     32
          "pgup"      33 "pageup" 33
          "pgdown"    34 "pagedown" 34
          "end"       35
          "home"      36
          "ins"       45 "insert" 45
          "del"       46 "delete" 46

          "left"      37
          "up"        38
          "right"     39
          "down"      40

          "*"         106
          "+"         107 "plus" 107 "kpplus" 107
          "kpminus"   109
          ";"         186
          "="         187
          ""          188
          "-"         189 "minus" 189
          "."         190
          "/"         191
          "`"         192
          "["         219
          "\\"        220
          "]"         221
          "'"         222
          }

         num-pad
         top-row
         f1-f24
         alphabet))

(def ^:private KNOWN-KEYS
  (u/project (fn [[k v]] [v k]) KEYS))

;; Data

(defonce BINDINGS (atom {}))
(defonce PRESSED (atom []))

(defonce ENABLED? (atom true))

;; Behavior

(defn button->code [button]
  (if-let [code (get KEYS button)]
    [code nil]
    [(get KEYS (str/lower-case button)) {:shift true}]))

(defn parse-chord [keystring]
  (let [bits   (str/split keystring #"-(?!$)")
        button (nth bits (-> bits count dec))
        [code mods] (button->code button)]
    (when-not code
      (throw (ex-info (str "Unknown key '" button
                           "' in keystring '" keystring "'")
                      {:keystring keystring})))
    (->> (drop-last bits)
         (map #(or (get MODS %)
                   (throw (ex-info (str "Unknown modifier '" mod
                                        "' in keystring '" keystring "'")
                                   {:keystring keystring}))))
         (reduce
           (fn [mods mod] (assoc mods mod true))
           (merge DEFCHORD {:code code} mods)))))

(defn parse [chain]
  (let [bits (str/split chain #" ")]
    (mapv parse-chord bits)))

(defn e->key [e]
  (get KNOWN-KEYS (.-keyCode e)))

(defn e->chord [e]
  (into {}
        (map (fn [[key attr]]
               [key (aget e attr)]))
        KEYATTRS))

(defn reset-sequence! []
  (swap! PRESSED empty))

(defn dispatch [e bindings]
  (let [chord    (e->chord e)
        sequence (conj @PRESSED chord)
        inner    (get-in bindings sequence)
        handlers (:handlers inner)]
    (cond
      (not inner) (reset-sequence!)
      handlers (do
                 (doseq [[_ handler] (:handlers inner)]
                   (handler e sequence))
                 (reset-sequence!))
      :else (reset! PRESSED sequence))))

(defn bind
  "Same as `bind!` just modifies `bindings` map you have to handle
  storage (like an atom) yourself."
  [bindings spec key cb]
  (let [parsed (parse spec)]
    (assoc-in bindings (conj parsed :handlers key) cb)))

(defn unbind
  "Same as `unbind!` just modifies `bindings` map you have to handle
  storage (like an atom) yourself."
  [bindings spec key]
  (let [parsed (parse spec)]
    (update-in bindings (conj parsed :handlers) dissoc key)))

;; Main external API

(defn bind!
  "Binds a sequence of button presses specified by `spec` to `cb` when
  pressed. Keys must be unique per `spec` and can be used to remove keybinding
  with `unbind!`.

  `spec` format is emacs-like strings a-la \"ctrl-c k\" \"meta-shift-k\" etc."
  [spec key cb]
  (swap! BINDINGS bind spec key cb))

(defn unbind!
  "Removes a callback identified by `key` from button sequence `spec`."
  [spec key]
  (swap! BINDINGS unbind spec key))

(defn unbind-all!
  "Remove all BINDINGS"
  []
  (reset-sequence!)
  (swap! BINDINGS empty))

(defn disable!
  "Disable dispatching of key events (but leave existing bindings intact)."
  []
  (reset! ENABLED? false))

(defn enable!
  "Enable dispatching of key events via the existing bindings."
  []
  (reset! ENABLED? true))

(defn dispatcher!
  "Return a function to be bound on `keydown` event preferably globally.
  Accepts atom with bindings.

  Is bound by default with `keycode/BINDINGS` atom so you don't need to use
  that."
  [bindings]
  (fn [e]
    (when (and @ENABLED? (e->key e))
      (dispatch e @bindings))))

;; Global key listener

(defonce bind-keypress-listener
  #?(:clj nil
     :cljs
          (js/addEventListener "keydown" (dispatcher! BINDINGS) false)))

;; Macros

#?(:clj
   (defmacro compile-time-parse-chord [chord]
     (parse-chord chord)))

#?(:clj
   (defmacro chord-case
     "Like `case` for key-chords, takes e (JS key event)
     and clauses of chords to expressions that will test again e's chord.

     Returns `false` when no chord matched.

     Use like:

     (chord-case e
       ctrl+c (println :pressed-control-c)

     Parses chords at compile time."
     [e & clauses]
     (let [parsed-chains (->> (partition 2 clauses)
                              (mapcat (fn [[chord expr]] [(parse-chord chord) expr])))]
       `(case (e->chord ~e)
          ~@parsed-chains
          false))))