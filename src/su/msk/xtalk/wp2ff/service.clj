(ns su.msk.xtalk.wp2ff.service
  (:require [ring.adapter.jetty :as jetty]
            [su.msk.xtalk.wp2ff.tools :as tools])
  (:gen-class))

(defn handler [request]
  (tools/inc-state :web-calls)
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    (str
             "<html><body>\n"
             "   <h2>Hello there!</h2>\n" ; (tools/env "GOOGLE_CLOUD_PROJECT") ", version " (tools/env "GAE_VERSION") "here."
             "   <p>Environment is:<br><pre>" (with-out-str (clojure.pprint/pprint (System/getenv))) "</pre></p>\n"
             "   <p>Request was:<br><pre>" (with-out-str (clojure.pprint/pprint request)) "</pre></p>\n"
             "   <p>State:<br><pre>" (with-out-str (clojure.pprint/pprint @tools/state)) "</pre></p>\n"
             "</html>")})

(defn start-web-service [config]
  (jetty/run-jetty handler
                   {:port (tools/env "PORT" (config :default-port))
                    :join? false}))
