(ns wp2ff.core
  (:require [clj-http.client :as http]
            [clojure.data.json :as json]
            [clojure.data.xml :as xml]
            [clojure.java.shell :as shell]
            [clojure.java.io :as io])
  (:import [java.io File])
  (:gen-class))

(def ^:dynamic *cfg*
  {:api.root "https://freefeed.net/"
   :seen.file "/var/tmp/wp2ff.seen"
   :cat-hash {"House" "домострой"}
   })

(defn create-session []
  "Session is just an auth token for now"
  (println "Logging to FreeFeed...")
  (let [res (http/post (str (*cfg* :api.root) "/v2/session")
                       {:form-params {:username (*cfg* :ff-user)
                                      :password (*cfg* :ff-pass)}
                        :throw-exceptions false})
        body (json/read-str (res :body))]
    (if (= (res :status) 200)
      {:token (body "authToken")}
      (do
        (println "Login failed:" (body "err"))
        nil))))

(defn create-attachment [session filename]
  (let [token (session :token)
        res (http/post (str (*cfg* :api.root) "/v1/attachments")
                       {:headers {"Authorization" (str "Bearer " token)}
                        :throw-exceptions false
                        :multipart [{:name "file"
                                     :content (clojure.java.io/file filename)}]})]
    (if (= (res :status) 200)
      (get-in (json/read-str (res :body)) ["attachments" "id"])
      (do
        (println "Cannot create attachment:" ((res :body) "err"))
        nil))))

(def seen-cache (atom nil))

(defn do-post [session post]
  (println "Posting to FreeFeed...")
  (let [attachments (mapv #(create-attachment session %) (post :images))
        feed (*cfg* :ff-user)
        req {:post {:body (post :text)
                    :attachments attachments}
             :meta {:feeds feed}}
        token (session :token)
        res (http/post (str (*cfg* :api.root) "/v1/posts")
                       {:content-type :json
                        :throw-exceptions false
                        :accept :json
                        :headers {"Authorization" (str "Bearer " token)}
                        :body (json/write-str req)})]
    (when-not (= (res :status) 200)
      (println "Post failed:" ((res :body) "err")))))

(defn mark-seen [post]
  (spit (*cfg* :seen.file) (str (post :link) "\n") :append true)
  (swap! seen-cache #(conj % (post :link))))

(defn seen-post? [post]
  (when (and (not @seen-cache)
             (.exists (io/file (*cfg* :seen.file))))
    (swap! seen-cache
           (fn [_]
             (with-open [r (io/reader (*cfg* :seen.file))]
               (doall (line-seq r))))))
  (and
   (seq @seen-cache)
   (.contains @seen-cache (post :link))))

(defn get-elements [item tag]
  (filter #(= (get % :tag) tag) item))

(defn get-element [item tag]
  (first (get-elements item tag)))

(defn get-content [item tag]
  (first (get (get-element item tag) :content)))

(defn get-contents [item tag]
  (->> tag
       (get-elements item)
       (map :content)
       (map first)))

(defn strip-query [s]
  (let [q (clojure.string/index-of s \?)]
    (if q
      (subs s 0 q)
      s)))

(defn cache-locally [url]
  "Fetch an image to local file, return local path"
  (let [res (http/get url
                      {:throw-exceptions false
                       :as :stream})]
    (when (and (= (res :status) 200)
               (= (get (res :headers) "Content-Type") "image/jpeg"))
      (let [file (File/createTempFile "wp2ff" ".jpeg")]
        (io/copy (res :body) file)
        (.getAbsolutePath file)))))

(defn get-post-images [item tag]
  (->> item
       (filter #(= (get % :tag) tag))
       (map #(get-in % [:attrs :url]))
       (filter #(clojure.string/includes? % "files.wordpress.com"))
       (map strip-query)
       (map cache-locally)
       (remove nil?)))

(defn html2text [html]
  (-> (shell/sh "pandoc" "-r" "html" "-w" "plain" :in html)
      (get :out)
      ;; Pandoc leaves brackets as an image placeholders
      (clojure.string/replace #"\n\[\]\n" "")))

(defn map-tags [tags]
  (->> tags
       (map #(get (*cfg* :cat-hash) %))
       (remove nil?)))

(defn footer [post]
  (format "Fed from !%s\n%s\n"
          (post :link)
          (->> (post :tags)
               (cons "wordpress")
               (map #(str "#" %))
               (clojure.string/join ", "))))

(defn post-eligible? [post]
  (.contains (map clojure.string/lower-case (post :tags)) "freefeed"))

(defn parse-feed [xml-string]
  ;;(spit "/tmp/feed.xml" xml-string)
  (let [xml (xml-seq (xml/parse-str xml-string))]
    (for [item xml :when (= (get item :tag) :item)]
      (let [content (get item :content)
            link (get-content content :link)
            tags (get-contents content :category)
            text (html2text (get-content content :encoded))
            imgs (get-post-images content :content)]
        (let [post {:link link
                    :text text
                    :tags tags
                    :images imgs}]
          (when (post-eligible? post)
            (let [tagged-post (assoc post :tags (map-tags (post :tags)))]
              (assoc post :text (str (post :text) "\n\n" (footer tagged-post))))))))))

(defn wp-feed []
  (println "Fetching Wordpress feed...")
  (let [res (http/get (*cfg* :RSS)
                      {:throw-exceptions false})]
    (if (= (res :status) 200)
      (remove nil? (parse-feed (res :body)))
      (do
        (println "Cannot get WP feed:" (res :reason-phrase))
        nil))))

(defn cleanup [post]
  (doall
   (for [image (post :images)]
     (io/delete-file image))))

(defn configure [args]
  (when-not (= (count args) 3)
    (println "Usage: wp2ff wordpress-user freefeed-user freefeed-password")
    (System/exit 1))
  (def ^:dynamic *cfg*
    (merge *cfg*
           {:RSS (format "https://%s.wordpress.com/feed/" (first args))
            :wp-user (first args)            
            :ff-user (second args)
            :ff-pass (nth args 2)})))

(defn -main [& args]
  (println "wp2ff v0.1, tobotras@gmail.com")
  (configure args)
  (when-let [session (create-session)]
    (doall
     (for [post (wp-feed)]
       (when-not (seen-post? post)
         (do-post session post)
         (mark-seen post)
         (cleanup post))))
    (println "Done, waiting to shut down.")))
