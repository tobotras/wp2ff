(ns su.msk.xtalk.wp2ff.core-test
  (:require [clojure.test :refer :all]
            [su.msk.xtalk.wp2ff.core :refer :all]
            [clojure.java.io :as io]
            [clj-http.client :as http]
            [next.jdbc :as jdbc]
            [honey.sql :as sql]
            [mockery.core :refer [with-mock with-mocks]]))

(deftest t-ff-session-fail 
  (with-mock m
    {:target ::http/post
     :return {:status 400
              :body "{\"err\": \"Horrible error happened\"}"}}
    (let [[res err] (create-ff-session)]
      (is (nil? res)))))

(deftest t-ff-session-ok
  (with-mock m
    {:target ::http/post
     :return {:status 200
              :body "{\"authToken\": \"thetoken\"}"}}
    (let [[session err] (create-ff-session)]
      (is (nil? err))
      (is (= session {:token "thetoken"})))))

(deftest t-post-attachment
  (when-let [session (first (create-ff-session))]
    (let [image-file "test.jpeg"]
      (let [[attach-id err] (create-attachment session image-file)]
        (if attach-id
          (drop-attachment session attach-id))
        (is attach-id)))))

(deftest t-not-post-attachment
  (when-let [session (first (create-ff-session))]
    (let [image-file "no.such.file"]
      (let [[attach-id err] (post-attachment session image-file)]
        (is (nil? attach-id))
        (is (= err "no.such.file (No such file or directory)")) ;; Fragile, ok
        ))))

(defn read-file [x]
  (with-open [in (clojure.java.io/input-stream x)
              out (java.io.ByteArrayOutputStream.)]
    (clojure.java.io/copy in out)
    (.toByteArray out)))

(def temp-files '())

(deftest t-cache-locally
  (let [test-file "test.jpeg"
        test-image (read-file test-file)]
    (with-mock m
      {:target ::http/get
       :return {:status 200
                :headers { "Content-Type" "image/jpeg" }
                :body test-image}}
      (let [file (cache-locally "http://any.url")
            cached-image (when file (read-file file))]
        (alter-var-root (var temp-files) #(conj % file))
        (is (= (seq test-image) (seq cached-image)))))))

(deftest t-not-cache-locally
  (with-mock m
    {:target ::http/get
     :return {:status 404}}
    (is (nil? (cache-locally "url")))))

(deftest t-strip-query
  (are [x y] (= x y)
    (strip-query "qwerty") "qwerty"
    (strip-query "qwerty?more") "qwerty"
    (strip-query "qwerty?more?andmore") "qwerty"))

(deftest t-footer
  (let [post {:link "link"
              :tags ["one" "two"]}]
    (is (= (footer post) "Fed from !link\n#wordpress, #one, #two\n"))))

;; ------------------------

(defn drop-temp-files []
  (doseq [file temp-files :when (some? file)]
    (io/delete-file file)))

(defn init-and-destroy [the-test]
  (configure)
  (init-db)
  (the-test)
  (drop-temp-files))

(use-fixtures :once init-and-destroy)
