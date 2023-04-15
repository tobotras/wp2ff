(ns su.msk.xtalk.wp2ff.core-test
  (:require [clojure.test :refer :all]
            [su.msk.xtalk.wp2ff.core :refer :all]
            [clojure.java.io :as io]
            [next.jdbc :as jdbc]
            [honey.sql :as sql]))

(deftest t-attachmenet
  (with-redefs [*cfg* (merge *cfg*
                             {:ff-user (System/getenv "USERNAME")
                              :ff-pass (System/getenv "PASSWORD")})]
    (if-let [session (create-session)]
      (let [image-file "test.qwe"]
        (spit image-file "qwerty")
        (let [attach-id (create-attachment session image-file)
              result (not (nil? attach-id))]
          (io/delete-file image-file)
          (is result)))
      nil)))
