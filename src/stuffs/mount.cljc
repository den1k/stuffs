(ns stuffs.mount
  (:require [mount.core :as mount])
  #?(:cljs (:require-macros [stuffs.mount])))

(defn start [& syms]
  (if (empty? syms)
    (mount/start)
    (if-let [states (not-empty (keep resolve syms))]
      (apply mount/start states)
      :noop)))

(defn stop [& syms]
  (if (empty? syms)
    (mount/stop)
    (if-let [states (not-empty (keep resolve syms))]
      (apply mount/stop states)
      :noop)))

(defmacro with-restart [state-syms & body]
  `(do
     (stop ~@state-syms)
     ~@body
     (start ~@state-syms)))

(comment

  (mount/defstate foo
    :start (println :start-foo)
    :stop (println :stop-foo))

  (macroexpand
    '(with-restart ['stuffs.mount/foo]
                   (println :doing)))

  (with-restart ['stuffs.mount/foo]
                (println :HHIII)
                (println :bye))
  )
