(defproject io.voix.fin/ib-client "1.0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.3.443"]
                 [com.interactivebrokers/tws-api "9.72.18"]
                 [clj-time "0.14.2"]
                 [factual/timely "0.0.3"]]

  :repl-options {:init-ns ib-client.core}
  :main ib-client.core
  :profiles
  {:uberjar {:aot :all}})
