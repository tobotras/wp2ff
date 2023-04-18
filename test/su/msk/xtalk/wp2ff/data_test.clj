(ns su.msk.xtalk.wp2ff.data-test
  (:require [clojure.test :refer :all]
            [su.msk.xtalk.wp2ff.data :refer :all]
            [clojure.java.io :as io]
            [next.jdbc :as jdbc]
            [honey.sql :as sql]))

(let [cnt (atom 0)]
  (defn make-test-post []
    {:link (str "test-" (swap! cnt inc))}))

(deftest t-mark-seen
  (let [post (make-test-post)]
    (mark-seen post)
    (is (seen-post? post))))

(deftest t-not-seen
  (is (nil? (seen-post? (make-test-post)))))

(defn drop-test-posts [the-test]
  (the-test)
  (sql! {:delete-from :seen
         :where [:like :link "test-%"]}))

(use-fixtures :once drop-test-posts)
