(ns su.msk.xtalk.wp2ff.data
  (:require [next.jdbc :as jdbc]
            [honey.sql :as sql]
            [clojure.tools.logging :as logg :only log])
  (:gen-class))

(def config {})

(defn init [cfg]
  (alter-var-root (var config) (constantly cfg)))

(defn sql! [request]
  (let [req (sql/format request)]
    (logg/logf :debug "SQL: '%s'" req)
    (jdbc/execute! config req)))

(defn get-state []
  (update-keys
   (first (sql! {:select [:*] :from :state}))
   #(keyword (name %))))

(defn inc-state [key]
  (logg/logf :debug "inc-state %s" (str key))
  (sql! {:update :state :set {key [:+ key 1]}}))

(defn mark-seen [post]
  (sql! {:insert-into :seen
         :columns [:link]
         :values [[(post :link)]]
         :on-conflict []
         :do-nothing true}))

(defn seen-post? [post]
  (seq (sql! {:select [:*] :from :seen :where [:= :link (post :link)]})))

(defn seen-total []
  ((first (sql! {:select :%count.* :from :seen})) :count))

(defn category->hashtag [category]
  (let [{tag :categories/ff_hashtag} (first (sql! {:select :ff_hashtag
                                                   :from :categories
                                                   :where [:= :wp_category category]}))]
    tag))

(defn logf [severity text & params]
  (let [message (apply format text params)]
    (sql! {:insert-into :log
           :columns [:severity :text]
           :values [[(name severity) message]]})
    (logg/log severity message)))
