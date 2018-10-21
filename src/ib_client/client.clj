(ns ib-client.client
  (:require
    ;; vendor
    [clojure.core.async :refer [chan <!! <! go-loop]]
    ;; local
    [ib-client.wrapper :refer [make-wrapper]]
    [ib-client.state :refer [->ApiState
                             new-req-id
                             close-req-chan!
                             create-req-chan!]]
    [ib-client.wrapper :as ew]
    [clj-time.core :as t])
  (:import [java.util List ArrayList]
           [com.ib.client EJavaSignal EClientSocket EReader Order
                          Contract ComboLeg TagValue ComboLeg]))

(def ^:private request-time (atom nil))

(defn start
  ([handlers client-id port]
   (start handlers client-id port))
  ([handlers client-id host port]
   (let [socket (atom nil)
         req-id (atom 0)
         reader-signal (atom (EJavaSignal.))
         general-handlers (atom {})
         req-id-handlers (atom {})
         api (->ApiState socket reader-signal req-id
                         general-handlers req-id-handlers)]
     (reset! general-handlers handlers)
     (reset! socket
             (EClientSocket.
               (make-wrapper api) @reader-signal))
     (.eConnect @socket host port client-id)
     (let [reader (EReader. @socket @reader-signal)]
       (.start reader)
       (future (loop [connected (.isConnected @socket)]
                 (.waitForSignal @reader-signal)
                 (when connected
                   (try
                     (do
                       (.processMsgs reader))
                     (catch Exception e
                       (do
                         (.getMessage e)
                         (.eDisconnect @socket))))
                   (recur (.isConnected @socket))))))
     api)))

(defn stop
  [api]
  (when (:socket api)
    (.eDisconnect @(:socket api))))

(defn create-contract
  [{:keys [^String symbol
           ^String sec-type
           ^String currency
           ^String exchange
           ^String ltd-or-cm
           ^String right
           ^double strike
           ^String multiplier
           ^Integer conid]
    :or {sec-type "STK"
         currency "USD"
         exchange ""}}]
  (doto (Contract.)
    (.symbol symbol)
    (.secType sec-type)
    (.currency currency)
    (.exchange exchange)
    (#(when conid (.conid % conid)))
    (#(when ltd-or-cm (.lastTradeDateOrContractMonth % ltd-or-cm)))
    (#(when right (.right % right)))
    (#(when strike (.strike % strike)))
    (#(when multiplier (.multiplier % multiplier)))))

(defn create-order
  [{:keys [^String action
           ^String order-type
           ^double total-quantity
           ^double limit-price
           ^double stop-price
           ^String tif
           ^boolean transmit
           ^String algo-strategy
           ^double trailing-percent
           ^String oca-group]
    :or {tif "DAY"
         transmit false}}]
  (doto (Order.)
    (.action action)
    (.orderType order-type)
    (#(when total-quantity (.totalQuantity % total-quantity)))
    (#(when limit-price (.lmtPrice % limit-price)))
    (#(when stop-price (.auxPrice % stop-price)))
    (#(when trailing-percent (.trailingPercent % trailing-percent)))
    (#(when oca-group (.ocaGroup % oca-group)))
    (#(when oca-group (.ocaType % 2)))
    (.tif tif)
    (.transmit transmit)
    (#(when algo-strategy
        (let [order
              (doto %
                (.algoStrategy algo-strategy))]
          (cond
            (= algo-strategy "Adaptive")
            (.algoParams
              order
              (ArrayList. [(TagValue. "adaptivePriority" "Patient")]))
            (= algo-strategy "ArrivalPx")
            (.algoParams
              order
              (ArrayList.
                [(TagValue. "maxPctVol" "0.1")
                 (TagValue. "riskAversion" "Neutral")
                 (TagValue. "startTime" "13:30:00 UTC")
                 (TagValue. "endTime" "19:59:00 UTC")
                 (TagValue. "forceCompletion" "0")
                 (TagValue. "allowPastEndTime" "0")]))))))))
                 ;(TagValue. "monetaryValue" "100000")]))))))))

(defn- read-results
  [req-chan]
  (loop [chunk (<!! req-chan)
         data []]
    (if chunk
      (recur (<!! req-chan) (conj data chunk))
      data)))

(defn ensure-connected
  [api]
  (when-not (.isConnected @(:socket api))
    (throw (ex-info "Connection error" {:error "IB API is not connected"}))))

(defn with-req-id
  [api req-fn]
  (ensure-connected api)
  (let [req-id (new-req-id api)
        req-chan (create-req-chan! api req-id)]
    (req-fn req-id)
    (read-results req-chan)))

;;
;; requests
;;

(defn request-contract
  [api contract-params]
  (loop []
    (let [last-request-second-passed
          (if @request-time
            (t/before? @request-time
                       (t/minus (t/now) (t/seconds 1)))
            true)]
      (if last-request-second-passed
        (first
          (with-req-id api
                       (fn [req-id]
                         (->> contract-params
                              create-contract
                              (.reqContractDetails @(:socket api) req-id)))))
        (recur)))))

(defn request-combo-contract
  [api contract-params legs-params]
  (let [contract (create-contract contract-params)
        all-legs
        (map (fn [{:keys [conid ratio action exchange]}]
               (doto (ComboLeg.)
                 (.conid conid)
                 (.ratio ratio)
                 (.action action)
                 (.exchange exchange)))
             legs-params)]
    (.comboLegs contract (ArrayList. all-legs))
    contract))

(defn request-option-contracts
  [api contract-params]
  (loop []
    (let [last-request-second-passed
          (if @request-time
            (t/before? @request-time
                       (t/minus (t/now) (t/seconds 1)))
            true)]
      (if last-request-second-passed
        (let [contract-response (request-contract api contract-params)]
          (if (ew/success-response? contract-response)
            (with-req-id api
                         (fn [req-id]
                           (.reqSecDefOptParams
                             @(:socket api)
                             req-id
                             (:symbol contract-params)
                             (:exchange contract-params)
                             "STK"
                             (.conid (-> contract-response
                                         :data
                                         :contract-details)))))))
        (recur)))))

(defn request-account-summary
  [api]
  (with-req-id api
               (fn [req-id]
                 (.reqAccountSummary @(:socket api)
                                     req-id
                                     "All"
                                     "$LEDGER"))))

(defn request-accounts
  [api]
  (ensure-connected api)
  (let [req-chan (create-req-chan! api :managed-accounts-req-id)]
    (.reqManagedAccts @(:socket api))
    (let [account-name (<!! req-chan)]
      (close-req-chan! api :managed-accounts-req-id)
      (:accounts (:data account-name)))))

(defn cancel-account-summary
  [api req-id]
  (ensure-connected api)
  (.cancelAccountSummary @(:socket api) req-id)
  (close-req-chan! api req-id))

(defn request-positions
  [api]
  (ensure-connected api)
  (let [req-chan (create-req-chan! api :position-req-id)]
    (.reqPositions @(:socket api))
    (read-results req-chan)))

(defn request-open-orders
  [api]
  (.reqOpenOrders @(:socket api)))

(defn request-market-data-type
  [api data-type]
  (ensure-connected api)
  (.reqMarketDataType @(:socket api) data-type))

(defn request-market-data
  [api ^Contract contract field-id]
  (ensure-connected api)
  (loop []
    (let [last-request-second-passed
          (if @request-time
            (t/before? @request-time
                       (t/minus (t/now) (t/seconds 1)))
            true)]
      (if last-request-second-passed
        (let [req-id (new-req-id api)
              req-chan (create-req-chan! api req-id)]
          (.reqMktData @(:socket api)
                       req-id
                       contract
                       field-id
                       false
                       nil)
          req-chan)
        (recur)))))

(defn cancel-market-data
  [api req-id]
  (ensure-connected api)
  (.cancelMktData @(:socket api) req-id)
  (close-req-chan! api req-id))

(defn request-realtime-bar
  [api contract req-id]
  (ensure-connected api)
  (loop []
    (let [last-request-second-passed
          (if @request-time
            (t/before? @request-time
                       (t/minus (t/now) (t/seconds 1)))
            true)]
      (if last-request-second-passed
        (let [req-chan (create-req-chan! api req-id)]
          (.reqRealTimeBars @(:socket api)
                            req-id
                            contract
                            5
                            "TRADES"
                            false
                            nil)
          req-chan)
        (recur)))))

(defn cancel-realtime-bar
  [api req-id]
  (ensure-connected api)
  (.cancelRealTimeBars @(:socket api) req-id)
  (close-req-chan! api req-id))

(defn request-historical-data
  [api contract
   {:keys [^String end-date-time
           ^String duration-str
           ^String bar-size-setting
           ^String what-to-show
           ^Integer use-rth
           ^Integer format-date
           ^List keep-u-t-date]
    :or {end-date-time ""
         what-to-show "TRADES"
         use-rth 1
         format-date 1
         keep-u-t-date nil} :as options}]
  (loop []
    (let [last-request-second-passed
          (if @request-time
            (t/before? @request-time
                       (t/minus (t/now) (t/millis 100)))
            true)]
      (if last-request-second-passed
        (let [contract-resp (request-contract api contract)]
          (if (ew/success-response? contract-resp)
            (when (-> contract-resp :data :contract-details)
              (with-req-id api
                           (fn [req-id]
                             (.reqHistoricalData @(:socket api)
                                                 req-id
                                                 (.contract
                                                   (:contract-details
                                                     (:data contract-resp)))
                                                 end-date-time
                                                 duration-str
                                                 bar-size-setting
                                                 what-to-show
                                                 use-rth
                                                 format-date
                                                 keep-u-t-date))))
            contract-resp))
        (recur)))))


(defn request-fundamental-data
  [api contract report-type]
  (loop []
    (let [last-request-second-passed
          (if @request-time
            (t/before? @request-time
                       (t/minus (t/now) (t/seconds 1)))
            true)]
      (if last-request-second-passed
        (let [contract-resp (request-contract api contract)]
          (if (ew/success-response? contract-resp)
            (when (-> contract-resp :data :contract-details)
              (first
                (with-req-id api
                             (fn [req-id]
                               (.reqFundamentalData @(:socket api)
                                                    req-id
                                                    (.contract
                                                      (:contract-details
                                                        (:data contract-resp)))
                                                    report-type)))))
            contract-resp))
        (recur)))))

(defn place-contract-order
  [api contract order-params order-id]
  (println "1" order-id)
  (println "2" contract)
  (println "3" order-params)
  (let [order (create-order order-params)]
    (println order)
    (.placeOrder @(:socket api)
                 order-id
                 contract
                 order)
    order-id))
