(ns ib-client.wrapper
  (:require
    ;; vendor
    [clojure.core.async :refer [chan >!! <!!]]
    ;; local
    [ib-client.state :refer [->ApiState
                             new-req-id
                             close-req-chan!
                             create-req-chan!]])
  (:import [com.ib.client EWrapper
                          ContractDetails
                          Contract
                          Order
                          Execution
                          CommissionReport
                          OrderState]
           (java.util Set)))

(defn success-response?
  [result]
  (if result
    (not= :error (:status result))
    false))

(defn- default-handler-fn
  [& args]
  (apply println "default: " args))

(defn- create-reponse-handler
  [req-id-channels general-handlers]
  (fn [k req-id ib-data]
    (let [data {:status k :data (assoc ib-data :req-id req-id)}]
      (if-let [req-id-channel (get @req-id-channels req-id)]
        (>!! req-id-channel data)
        (if (contains? @general-handlers k)
          ((get @general-handlers k) data)
          (default-handler-fn data))))))

(def ^:private
  exec-details-atom
  (atom nil))

(defn make-wrapper
  [api]
  (let [handle (create-reponse-handler (:req-id-channels api)
                                       (:general-handlers api))]
    (reify EWrapper
      (^void error [_ ^String err]
        (handle :error 0 {:error-msg err}))

      (^void error [_ ^Exception ex]
        (handle :error 0 {:error-msg (.getMessage ex)}))

      (^void error [_ ^int id ^int error-code ^String error-msg]
        (handle :error id {:id id
                           :error-code error-code
                           :error-msg error-msg})
        (close-req-chan! api id))

      (^void nextValidId [_ ^int id]
        (reset! (:req-id api) id))

      (^void connectAck [_]
        (.startAPI @(:socket api)))

      (^void connectionClosed [_]
        (default-handler-fn "Stopping api"))

      (^void accountSummary [_ ^int req-id
                             ^String account
                             ^String tag
                             ^String value
                             ^String currency]
        (handle :account-summary
                req-id
                {:account account
                 :tag tag
                 :value value
                 :currency currency}))

      (^void accountSummaryEnd [_ ^int req-id]
        (.cancelAccountSummary @(:socket api) req-id)
        (close-req-chan! api req-id))

      (^void position [_ ^String account
                       ^Contract contract
                       ^double pos
                       ^double avgCost]
        (handle :position
                :position-req-id
                {:account account
                 :contract contract
                 :pos pos
                 :avgCost avgCost}))

      (^void positionEnd [_]
        (when (get @(:req-id-channels api) :position-req-id)
            (close-req-chan! api :position-req-id)))

      (^void openOrder [_ ^int order-id
                        ^Contract contract
                        ^Order order
                        ^OrderState orderState]
        (handle :open-order
                order-id
                {:contract contract
                 :order order
                 :status (.getStatus orderState)}))

      (^void orderStatus [_ ^int order-id ^String status ^double filled
                          ^double remaining ^double avgFillPrice ^int permId
                          ^int parentId ^double lastFillPrice ^int clientId
                          ^String whyHeld]
        (handle :order-status
                order-id
                {:status status
                 :filled filled
                 :remaining remaining
                 :avg-fill-price avgFillPrice
                 :per-id permId
                 :parent-id parentId
                 :last-fill-price lastFillPrice
                 :client-id clientId
                 :why-held whyHeld}))

      (^void execDetails [_ ^int req-id ^Contract contract ^Execution execution]
        (reset! exec-details-atom req-id)
        (handle :exec-details
                req-id
                {:contract contract
                 :execution execution}))

      (^void execDetailsEnd [_ ^int req-id])

      (^void commissionReport [_ ^CommissionReport commissionReport]
        (handle :commissions
                @exec-details-atom
                {:commissions commissionReport}))

      (^void openOrderEnd [_])

      (^void currentTime [_ ^long time]
        (default-handler-fn time))

      (^void managedAccounts [_ ^String acc-name]
        (handle :managed-accounts
                :managed-accounts-req-id
                {:accounts acc-name}))

      (^void contractDetails [_ ^int req-id ^ContractDetails contract-details]
        (handle :contract-details
                req-id
                {:contract-details contract-details}))

      (^void contractDetailsEnd [_ ^int req-id]
        (close-req-chan! api req-id))

      (^void securityDefinitionOptionalParameter [_ ^int req-id
                                                  ^String exchange
                                                  ^int underlyingConId
                                                  ^String tradingClass
                                                  ^String multiplier
                                                  ^Set expirations
                                                  ^Set strikes]
        (handle :sec-def-opt-params
                req-id
                {:exchange exchange
                 :underlying-con-id underlyingConId
                 :trading-class tradingClass
                 :multiplier multiplier
                 :expirations (set expirations)
                 :strikes (set strikes)}))

      (^void securityDefinitionOptionalParameterEnd [_ ^int req-id]
        (close-req-chan! api req-id))

      (^void historicalData [_ ^int req-id
                             ^String date
                             ^double open
                             ^double high
                             ^double low
                             ^double close
                             ^int volume
                             ^int count
                             ^double WAP
                             ^boolean has-gaps]
        (if (= -1 volume)
          (close-req-chan! api req-id)
          (handle :historical-data
                  req-id
                  {:req-id req-id
                   :date date
                   :open open
                   :high high
                   :low low
                   :close close
                   :volume volume
                   :count count
                   :WAP WAP
                   :has-gaps has-gaps})))

      (^void marketDataType [_ ^int new-req-id ^int market-data-type])

      (^void tickOptionComputation [_ ^int tickerId
                                    ^int field
                                    ^double impliedVol
                                    ^double delta
                                    ^double optPrice
                                    ^double pvDividend,
                                    ^double gamma
                                    ^double vega
                                    ^double theta
                                    ^double undPrice]
        (when (and (not= Double/MAX_VALUE delta)
                   (not= Double/MAX_VALUE undPrice))
          (handle :option-computation
                  tickerId
                  {:field field
                   :implied-vol impliedVol
                   :delta delta
                   :opt-price optPrice
                   :pv-dividend pvDividend,
                   :gamma gamma
                   :vega vega
                   :theta theta
                   :und-price undPrice})))

      (^void tickPrice [_ ^int tickerId
                        ^int field
                        ^double price
                        ^int attribs]
        (handle :tick-price
                tickerId
                {:field field
                 :price price
                 :attribs attribs}))

      (^void tickSize [_ ^int tickerId
                       ^int field
                       ^int size])

      (^void tickString [_ ^int tickerId
                         ^int tickType
                         ^String value])

      (^void tickGeneric [_ ^int tickerId
                          ^int tickType
                          ^double value])

      (^void realtimeBar [_ ^int req-id
                          ^long time
                          ^double open
                          ^double high
                          ^double low
                          ^double close
                          ^long volume
                          ^double wap
                          ^int count]
        (handle :real-time-bar
                req-id
                {:time time
                 :open open
                 :high high
                 :low low
                 :close close
                 :volume volume
                 :wap wap
                 :count count}))

      (^void fundamentalData [_ ^int req-id ^String data]
        (handle :fundamental-data
                req-id
                {:xml data})
        (close-req-chan! api req-id)))))
