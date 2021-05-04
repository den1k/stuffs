(ns stuffs.js-interop
  #?(:cljs
     (:require [applied-science.js-interop :as j]))
  (:refer-clojure :exclude [get get-in assoc! assoc-in! update! update-in! select-keys contains? unchecked-get unchecked-set apply]))

(defn- noop [& _])

(def get #?(:clj noop :cljs j/get))
(def get-in #?(:clj noop :cljs j/get-in))

(def assoc! #?(:clj noop :cljs j/assoc!))
(def assoc-in! #?(:clj noop :cljs j/assoc-in!))

(def update! #?(:clj noop :cljs j/update!))
(def update-in! #?(:clj noop :cljs j/update-in!))

(def call #?(:clj noop :cljs j/call))
(def call-in #?(:clj noop :cljs j/call-in))

(def apply #?(:clj noop :cljs j/apply))
(def apply-in #?(:clj noop :cljs j/apply-in))

(def lookup #?(:clj noop :cljs j/lookup))
