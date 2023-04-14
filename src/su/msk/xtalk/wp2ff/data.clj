(ns su.msk.xtalk.wp2ff.data
  (:require [next.jdbc :as jdbc]
            [honey.sql :as sql])
  (:gen-class))

(def config (atom {}))

(defn init [cfg]
  (swap! config (fn [_] cfg)))

(defn sql! [request]
  (jdbc/execute! @config (sql/format request)))

(defn get-state []
  (sql! {:select [:*] :from :state}))

(defn inc-state [key]
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
