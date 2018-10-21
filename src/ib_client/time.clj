(ns ib-client.time
  (:require
    [clj-time.core :as t]
    [clj-time.coerce :as c]))

(def ^:private NY_TZ
  (t/time-zone-for-id "America/New_York"))

(def ^:private UTC_TZ
  (t/time-zone-for-id "UTC"))

(defn extract-year-month-day
  [date-time]
  (t/date-time
    (.getYear date-time)
    (.getMonthOfYear date-time)
    (.getDayOfMonth date-time)
    0
    0))

(defn ny-time-in-local-tz
  [hours minutes timezone date-time]
  (-> (if date-time
        (extract-year-month-day date-time)
        (t/now))
      (.withHourOfDay hours)
      (.withMinuteOfHour minutes)
      (.withSecondOfMinute 0)
      (.withMillisOfSecond 0)
      (t/from-time-zone NY_TZ)
      (t/to-time-zone (if timezone
                        timezone
                        (t/default-time-zone)))))

(defn current-time-within-ny-time?
  [h1 m1 h2 m2]
  (let [start (ny-time-in-local-tz h1 m1 nil nil)
        end (ny-time-in-local-tz h2 m2 nil nil)]
    (t/within? (t/interval start end) (t/now))))

(defn session-end?
  [date-time]
  (let [end (ny-time-in-local-tz 16 00 UTC_TZ date-time)]
    (.isEqual end date-time)))

(defn session-start?
  [date-time]
  (let [start (ny-time-in-local-tz 9 30 UTC_TZ date-time)]
    (.isEqual start date-time)))

(defn session-start-in-stamp
  []
  (c/to-epoch (ny-time-in-local-tz 9 30 UTC_TZ nil)))

(defn session-open?
  []
  (current-time-within-ny-time? 9 30 16 0))

(defn trading-hours?
  []
  (current-time-within-ny-time? 9 40 15 30))

(defn not-session-open?
  []
  (if (or (= (t/day-of-week (t/now)) 7) (= (t/day-of-week (t/now)) 6))
    true
    (not (current-time-within-ny-time? 8 30 16 30))))
