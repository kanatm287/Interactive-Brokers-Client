(ns ib-client.state
  (:require
    ;; vendor
    [clojure.core.async :refer [chan close!]]))

(defprotocol Api
  (new-req-id [self])
  (create-req-chan!
    [self req-id]
    [self req-id buffer])
  (close-req-chan! [self req-id]))

(defrecord ApiState [socket
                     reader-signal
                     req-id
                     general-handlers
                     req-id-channels]
  Api
  (new-req-id
    [{r :req-id}]
    (swap! r inc))
  (create-req-chan!
    [api-state req-id]
    (create-req-chan! api-state req-id nil))
  (create-req-chan!
    [{rc :req-id-channels} req-id buffer]
    (let [req-chan (if buffer (chan buffer) (chan))]
      (swap! rc #(assoc % req-id req-chan))
      req-chan))
  (close-req-chan!
    [{rc :req-id-channels} req-id]
    (let [c (get @rc req-id)]
      (when c (close! c))
      (swap! rc #(dissoc % req-id)))))
