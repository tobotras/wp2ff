(ns su.msk.xtalk.wp2ff.core
  (:require [clj-http.client :as http]
            [clojure.data.json :as json]
            [clojure.data.xml :as xml]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log :refer :all]
            [su.msk.xtalk.wp2ff.tools :as tools]
            [su.msk.xtalk.wp2ff.data :as data]
            [su.msk.xtalk.wp2ff.service :as service])
  (:import [java.io File])
  (:gen-class))

(def ^:dynamic *cfg*
  {:api.root      "https://freefeed.net/"
   :cat-hash      {"House" "домострой"}
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
      (do
        (data/inc-state :ff_sessions)
        {:token (body "authToken")})
      (do
        (data/inc-state :ff_errors)
        (log/error "Login failed:" (body "err"))
        nil))))

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
        (log/error "Cannot create attachment:" ((res :body) "err"))
        nil))))

(defn do-post [session post]
  "Returns post link on success, nil otherwise"
  (log/info "Posting to FreeFeed, entry:" (post :link))
  (let [attachments (mapv #(create-attachment session %) (post :imgs))
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
    (if (= (res :status) 200)
      (do
        (data/inc-state :ff_posts)
        (log/info "Post succeeded")
        (post :link))
      (do
        (data/inc-state :ff_errors)
        (log/error "Post failed:" ((res :body) "err"))
        nil))))

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
       (map #(get (*cfg* :cat-hash) %))
       (remove nil?)))

(defn footer [link tags]
  (format "Fed from !%s\n%s\n"
          link
          (->> tags
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
          (let [text (str (tools/html->text (get-content content :encoded))
                          "\n\n"
                          (footer link tags))]
            (log/debug "Text to be posted:" text)
            (merge post
                   {:tags (map-tags tags)
                    :imgs (get-post-images content :content)
                    :text text})))))))

(defn wp-feed []
  (log/info "Fetching Wordpress feed...")
  (let [res (http/get (*cfg* :RSS)
                      {:throw-exceptions false})]
    (if (= (res :status) 200)
      (do
        (data/inc-state :wp_sessions)
        (remove nil? (parse-feed (res :body))))
      (do
        (data/inc-state :wp_errors)
        (log/error "Cannot get WP feed:" (res :reason-phrase))
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
    result))

(defn wp-to-ff [params]
  "Called from GAE cron job"
  (log/info "Next pass started, params:" (with-out-str (clojure.pprint/pprint params)))
  (if-let [session (create-session)]
    (do
      (log/debug "Got FF session")
      (let [processed-posts (mapv #(process-post session %) (wp-feed))]
        (if (some nil? processed-posts)
          {:status 500
           :body "Some post processing error occured, sorry"}
          {:status 200
           :body (if (not-empty processed-posts)
                   (str "New entries posted: "
                        (clojure.string/join ", " processed-posts))
                   "No new entries posted")})))
    {:status 500
     :body "Cannot establish FreeFeed session"}))

(defn -main [& args]
  (log/info "wp2ff v0.1, tobotras@gmail.com")
  (configure)
  (log/debug "Set everything, serving")
  (service/start-web-service *cfg* wp-to-ff))
