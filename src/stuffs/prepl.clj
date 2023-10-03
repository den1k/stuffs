(ns stuffs.prepl
  (:require [clojure.core.server :as s]
            [missionary.core :as m]
            [babashka.process :as p]
            [mount.core :as mount :refer [defstate]])
  (:import (java.io PipedReader PipedWriter)
           (java.util.concurrent Executors)))

; https://gist.github.com/leonoel/d13dd1b8045b3167dbf688f57244d221

(defn remote-prepl "
Returns a function taking a `java.io.PipedWriter` and returning a flow connecting to a remote prepl on given `host` and
`port`, sending the content of the pipe to the remote peer and emitting received evaluation results. The pipe is closed
on flow cancellation.
" [host port]
  (fn [^PipedWriter pipe]
    (m/observe
      (fn [cb]
        (.start
          (Thread.
            (reify Runnable
              (run [_]
                (s/remote-prepl host port
                                (PipedReader. pipe) cb)))))
        #(.close pipe)))))

(defn write-form "
Returns a task writing the edn representation of given `form` to given `java.io.Writer`.
" [executor writer form]
  (m/via executor
         (binding [*out* writer]
           (prn form))))

(defn send-forms "
Returns a flow serializing and writing forms produced by flow `forms` to a `java.io.PipedWriter`, and concurrently
running the flow returned by the `channel` function, called with the pipe.
" [channel forms]
  (m/ap
    (let [writer (PipedWriter.)]
      (m/amb= (m/?> (channel writer))
              (do (m/? (write-form (Executors/newSingleThreadExecutor)
                                   writer (m/?> forms))) (m/amb))))))

(defn process-tuples "
Returns a flow processing each element of tuples produced by given flow `input` with a separate flow processor.
The flow processors `ps` must be a sequence matching the shape of expected tuples. Resulting tuples are collapsed with
given function `f`.
" [f ps input]
  (let [>input (m/stream input)]
    (->> ps
         (map-indexed (fn [i p]
                        (p (m/sample #(nth % i) >input))))
         (apply m/zip f))))

(declare remote-eval!)

(defn prepl-task-with-port-forwarding [{:keys [ssh-remote ssh-remote-port]}]
  (let [local-port ssh-remote-port
        prepl-task (->> (m/observe
                          (fn [!]
                            (let [port-forwarding
                                  (p/process
                                    {:out :inherit}
                                    "ssh" ssh-remote "-L" (str local-port ":172.17.0.2:" ssh-remote-port))]
                              ; (p/alive? pf)
                              (defn remote-eval! [x]
                                (let [d (m/dfv)]
                                  (! [d x])
                                  (m/? d)))
                              #(do
                                 (p/destroy port-forwarding)
                                 (def remote-eval! nil)))))
                        (process-tuples (fn [d x] (d x))
                                        [identity
                                         (partial send-forms
                                                  (remote-prepl
                                                    "localhost"
                                                    local-port))])
                        (m/reduce {} nil))]
    (prepl-task
      (fn [_] (println "Remote prepl terminated."))
      (fn [^Throwable e] (.printStackTrace e)))
    ))

(comment

  (defstate prepl-task
    :start (prepl-task-with-port-forwarding
             {:ssh-remote      "user@domain.com"
              :ssh-remote-port 1234})
    :stop (prepl-task))
  (mount/start)
  (mount/stop #'prepl-task)

  (remote-eval! '(rand))
  )
