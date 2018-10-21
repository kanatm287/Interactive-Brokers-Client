(ns ib-client.utils
  (:require
    ;; vendor
    [clj-time.core :as t]
    [clj-time.format :as f]
    ;; local
    [ib-client.client :as client]
    [ib-client.wrapper :as wrapper]))

;; ----------------=========== Historical data utils ===========----------------

(def custom-formatter (f/formatter :basic-date))

(def custom-formatter-time (f/formatter :hour-minute-second))

(defn date-time-now
  [date-time]
  (str (f/unparse custom-formatter date-time)
       "  "
       (f/unparse custom-formatter-time date-time)))

(defn request-ticker-historical-data
  [api ticker bar-size days end-date-time]
  (let [request-fn #(client/request-historical-data
                      api
                      {:symbol ticker
                       :exchange "SMART"}
                      {:end-date-time (if end-date-time
                                        end-date-time
                                        (date-time-now (t/now)))
                       :duration-str days
                       :bar-size-setting bar-size
                       :what-to-show "TRADES"})]
    (loop [tries 0
           resp (request-fn)]
      (if (wrapper/success-response? resp)
        resp
        (when (< tries 5)
          (Thread/sleep 5000)
          (recur (inc tries) (request-fn)))))))

(defn request-ticker-fundamental-data
  [api ticker]
  (let [request-fn #(client/request-fundamental-data
                      api
                      {:symbol ticker
                       :exchange "SMART"}
                      "RESC")]
    (loop [tries 0
           resp (request-fn)]
      (if (wrapper/success-response? resp)
        (:data resp)
        (when (< 10 tries)
          (Thread/sleep 10000)
          (recur (inc tries) (request-fn)))))))
