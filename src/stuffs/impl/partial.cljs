(ns stuffs.impl.partial
  "from https://github.com/reagent-project/reagent/blob/c214466bbcf099eafdfe28ff7cb91f99670a8433/src/reagent/impl/util.cljs"
  )

(deftype PartialFn [pfn f args]
  Fn
  IFn
  (-invoke [_]
    (pfn))
  (-invoke [_ a]
    (pfn a))
  (-invoke [_ a b]
    (pfn a b))
  (-invoke [_ a b c]
    (pfn a b c))
  (-invoke [_ a b c d]
    (pfn a b c d))
  (-invoke [_ a b c d e]
    (pfn a b c d e))
  (-invoke [_ a b c d e f]
    (pfn a b c d e f))
  (-invoke [_ a b c d e f g]
    (pfn a b c d e f g))
  (-invoke [_ a b c d e f g h]
    (pfn a b c d e f g h))
  (-invoke [_ a b c d e f g h i]
    (pfn a b c d e f g h i))
  (-invoke [_ a b c d e f g h i j]
    (pfn a b c d e f g h i j))
  (-invoke [_ a b c d e f g h i j k]
    (pfn a b c d e f g h i j k))
  (-invoke [_ a b c d e f g h i j k l]
    (pfn a b c d e f g h i j k l))
  (-invoke [_ a b c d e f g h i j k l m]
    (pfn a b c d e f g h i j k l m))
  (-invoke [_ a b c d e f g h i j k l m n]
    (pfn a b c d e f g h i j k l m n))
  (-invoke [_ a b c d e f g h i j k l m n o]
    (pfn a b c d e f g h i j k l m n o))
  (-invoke [_ a b c d e f g h i j k l m n o p]
    (pfn a b c d e f g h i j k l m n o p))
  (-invoke [_ a b c d e f g h i j k l m n o p q]
    (pfn a b c d e f g h i j k l m n o p q))
  (-invoke [_ a b c d e f g h i j k l m n o p q r]
    (pfn a b c d e f g h i j k l m n o p q r))
  (-invoke [_ a b c d e f g h i j k l m n o p q r s]
    (pfn a b c d e f g h i j k l m n o p q r s))
  (-invoke [_ a b c d e f g h i j k l m n o p q r s t]
    (pfn a b c d e f g h i j k l m n o p q r s t))
  (-invoke [_ a b c d e f g h i j k l m n o p q r s t rest]
    (apply pfn a b c d e f g h i j k l m n o p q r s t rest))
  IEquiv
  (-equiv [_ ^clj other]
    (and (instance? PartialFn other)
         (= f (.-f other))
         (= args (.-args other))))
  IHash
  (-hash [_] (hash [f args])))

(defn make-partial-fn [f args]
  (->PartialFn (apply partial f args) f args))