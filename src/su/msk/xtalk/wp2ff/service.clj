(ns su.msk.xtalk.wp2ff.service
  (:require [ring.adapter.jetty :as jetty]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [clojure.tools.logging :as log :refer :all]
            [su.msk.xtalk.wp2ff.data :as data]
            [su.msk.xtalk.wp2ff.tools :as tools])
  (:gen-class))

(defn version []
  (str (tools/env "GOOGLE_CLOUD_PROJECT" "Standalone wp2ff") ", version " (tools/env "GAE_VERSION" "0.0")))

(defn render-state [request]
  (data/inc-state :web_calls)
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    (str
             "<html><body>\n"
             "   <h2>Hello there!</h2>\n" (version) "here."
             "   <p>Request was:<br><pre>"
             (with-out-str (clojure.pprint/pprint request)) "</pre></p>\n"
             "   <p>State:<br><pre>"
             (with-out-str (clojure.pprint/pprint (data/get-state))) "</pre></p>\n"
             "</html>")})

(defn handle-job [request]
  {:status 200
   :headers {"Content-Type" "text/plain"}
   :body (str "Request:\n" (with-out-str (clojure.pprint/pprint request))) })

(defn start-web-service [port callback]
  (defroutes app
    (GET "/state"   [] render-state)
    (GET "/job/:job" [job] (callback job))
    (route/not-found "<h1>Page not found</h1>"))
  (jetty/run-jetty app
                   {:port port
                    :join? true}))
