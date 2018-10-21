(ns ib-client.core
  (:require
    ;; local
    [ib-client.client :as client])

  (:gen-class))

(defn start
  ([client-id handlers port]
   (start client-id handlers "localhost" port))
  ([client-id handlers host port]
   (client/start handlers client-id host port)))

(defn stop
  [api]
  (client/stop api))
