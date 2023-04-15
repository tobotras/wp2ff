(ns su.msk.xtalk.wp2ff.core
  (:require [clj-http.client :as http]
            [clojure.data.json :as json]
            [clojure.data.xml :as xml]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log :refer :all]
            [clojure.set :as cset]
            [su.msk.xtalk.wp2ff.tools :as tools]
            [su.msk.xtalk.wp2ff.data :as data]
            [su.msk.xtalk.wp2ff.service :as service])
  (:import [java.io File])
  (:gen-class))

(def ^:dynamic *cfg*
  {:api.root      "https://freefeed.net/"
   :default-port  8080})

(defn create-session []
  "Session is just an auth token for now"
  (log/info "Logging to FreeFeed...")
  (let [res (http/post (str (*cfg* :api.root) "/v2/session")
                       {:form-params {:username (*cfg* :ff-user)
                                      :password (*cfg* :ff-pass)}
                        :throw-exceptions false})
        body (json/read-str (res :body))]
    (if (= (res :status) 200)
      [{:token (body "authToken")} nil]
      [nil (body "err")])))

(defn create-attachment [session filename]
  (let [token (session :token)
        res (http/post (str (*cfg* :api.root) "/v1/attachments")
                       {:headers {"Authorization" (str "Bearer " token)}
                        :throw-exceptions false
                        :multipart [{:name "file"
                                     :content (clojure.java.io/file filename)}]})]
    (if (= (res :status) 200)
      (do
        (data/inc-state :ff_images)
        (get-in (json/read-str (res :body)) ["attachments" "id"]))
      (do
        (data/inc-state :ff_errors)
        (data/logf :error "Cannot create attachment: %s" ((res :body) "err"))
        nil))))

(defn do-post [session post]
  (log/info "Posting to FreeFeed, entry:" (post :link))
  (let [attachments (mapv #(create-attachment session %) (post :imgs))]
    (when-not (some nil? attachments)
      (let [feed (*cfg* :ff-user)
            req {:post {:body (post :text)
                        :attachments attachments}
                 :meta {:feeds feed}}
            token (session :token)
            res (http/post (str (*cfg* :api.root) "/v1/posts")
                           {:content-type :json
                            :throw-exceptions false
                            :accept :json
                            :headers {"Authorization" (str "Bearer " token)}
                            :body (json/write-str req)})
            success? (= (res :status) 200)]
        (data/inc-state (if success? :ff_posts :ff_errors))
        (if success?
          (data/logf :info "Posted %s to FF" (post :link))
          (data/logf :error "Post of %s failed: %s" (post :link) ((res :body) "err")))
        success?))))
    
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

(defn map-tags [tags]
  (->> tags
       (map data/category->hashtag)
       (remove nil?)))

(defn footer [post]
  (format "Fed from !%s\n%s\n"
          (post :link)
          (->> (post :tags)
               (cons "wordpress")
               (map #(str "#" %))
               (clojure.string/join ", "))))

(defn post-eligible? [post]
  (and 
   (.contains (map clojure.string/lower-case (post :tags)) "freefeed")
   (not (data/seen-post? post))))

(defn parse-feed [xml-string]
  ;;(spit "/tmp/feed.xml" xml-string)
  (let [xml (xml-seq (xml/parse-str xml-string))]
    (for [item xml :when (= (get item :tag) :item)]
      (let [content (get item :content)
            tags (get-contents content :category)
            link (get-content content :link)
            post {:tags tags
                  :link link}]
        (when (post-eligible? post)
          (let [post (assoc post :tags (map-tags tags))
                text (str (tools/html->text (get-content content :encoded))
                          "\n\n"
                          (footer post))]
            (log/debug "Text to be posted:" text)
            (merge post
                   {:imgs (get-post-images content :content)
                    :text text})))))))

(defn wp-feed []
  (log/info "Fetching Wordpress feed...")
  (let [res (http/get (*cfg* :RSS)
                      {:throw-exceptions false})]
    (if (= (res :status) 200)
      (let [entries (parse-feed (res :body))
            new-entries (remove nil? entries)]
        (data/inc-state :wp_sessions)
        (data/logf :debug "Fetched %d entries, %d old, %d new"
                     (count entries)
                     (count (remove #(not (nil? %)) entries))
                     (count new-entries))
        new-entries)
      (do
        (data/inc-state :wp_errors)
        (data/logf :error "Cannot fetch WP feed: %s" (res :reason-phrase))
        nil))))

(defn cleanup [post]
  (doall
   (for [image (post :imgs)]
     (io/delete-file image))))

(defn configure []
  (let [wp-user (tools/env "WP_USER")
        ff-user (tools/env "FF_USER")
        ff-pass (tools/env "FF_PASS")]
    (when (some nil? [wp-user ff-user ff-pass])
      (log/error "Environment not set")
      (System/exit 1))
    (def ^:dynamic *cfg*
      (merge *cfg*
             {:RSS (format "https://%s.wordpress.com/feed/" wp-user)
              :wp-user wp-user
              :ff-user ff-user
              :ff-pass ff-pass})))
  (data/init {:dbtype "postgresql"
              :dbname   (tools/env "DB_NAME" "wp2ff")
              :host     (tools/env "DB_HOST" "localhost")
              :user     (tools/env "DB_USER" "wp2ff")
              :password (tools/env "DB_PASS" "wp2ffpass")}))
  
(defn process-post [session post]
  (let [result (when-let [post-result (do-post session post)]
                 (data/mark-seen post)
                 post-result)]
    (cleanup post)
    (assoc post :state result)))

(defn wp-to-ff [params]
  "Called from GAE cron job"
  (log/info "Next pass started, params:" (with-out-str (clojure.pprint/pprint params)))
  (let [[session err] (create-session)]
    (if session
      (do
        (data/inc-state :ff_sessions)
        (data/logf :debug "Created new FF session")
        (let [processed-posts (map #(process-post session %) (wp-feed))
              failed-posts (remove #(get % :state) processed-posts)
              done-posts (vec (cset/difference (set processed-posts) (set failed-posts)))
              status (if (empty? failed-posts) (if (empty? done-posts) 200 201) 500)]
          {:status status
           :body (if (empty? processed-posts)
                   "No posts to process"
                   (str (when-not (empty? done-posts)
                          (format "Posted %d posts: %s\n" (count done-posts)
                                  (clojure.string/join ", " (map #(get % :link) done-posts))))
                        (when-not (empty? failed-posts)
                          (format "Failed %d posts: %s\n" (count failed-posts)
                                  (clojure.string/join ", " (map #(get % :link) failed-posts))))))}))
      (do
        (data/inc-state :ff_errors)
        (data/logf :error "FF login failed: %s" err)
        {:status 500
         :body "Cannot establish FreeFeed session"}))))

(defn -main [& args]
  (log/info "wp2ff v0.1, tobotras@gmail.com")
  (configure)
  (log/debug "Set everything, serving")
  (service/start-web-service *cfg* wp-to-ff))
