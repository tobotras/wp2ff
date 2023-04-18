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
  (:import [java.io File]
           [java.util Base64])
  (:gen-class))

(defmacro ppr [args]
  `(with-out-str (clojure.pprint/pprint ~args)))

(def the-config
  {:api.root "https://freefeed.net/"
   :port     8080})

(defn CFG [key]
  (if (contains? the-config key)
    (get the-config key)
    (throw (Exception. (str "Reference to unknown parameter: " key)))))

(defn create-ff-session
  "Session is just an auth token for now"
  []
  (log/info "Logging to FreeFeed...")
  (let [res (http/post (str (CFG :api.root) "/v2/session")
                       {:form-params {:username (CFG :ff-user)
                                      :password (CFG :ff-pass)}
                        :throw-exceptions false})
        body (json/read-str (res :body))]
    (if (= (res :status) 200)
      [{:token (body "authToken")} nil]
      [nil (body "err")])))

(defn post-attachment [session filename]
  (try
    (let [token (session :token)
          res (http/post (str (CFG :api.root) "/v1/attachments")
                         {:headers {"Authorization" (str "Bearer " token)}
                          :throw-exceptions false
                          :multipart [{:name "file"
                                       :content (clojure.java.io/file filename)}]})]
      (if (= (res :status) 200)
        [(get-in (json/read-str (res :body)) ["attachments" "id"]) nil]
        [nil ((res :body) "err")]))
    (catch java.io.FileNotFoundException e
      [nil (ex-message e)])))

(defn create-attachment [session filename]
  (let [[id err] (post-attachment session filename)]
    (if id
      (data/inc-state :ff_images)
      (do
        (data/inc-state :ff_errors)
        (data/logf :error "Cannot create attachment: %s" err)))))

(defn drop-attachment [session id]
  ;; Don't know if it's possible
  true)

(defn do-ff-post [session post]
  (log/infof "Posting to FreeFeed, entry: '%s'" (post :link))
  (let [attachments (mapv #(create-attachment session %) (post :imgs))]
    (when-not (some nil? attachments)
      (let [feed (CFG :ff-user)
            req {:post {:body (post :text)
                        :attachments attachments}
                 :meta {:feeds feed}}
            token (session :token)
            res (http/post (str (CFG :api.root) "/v1/posts")
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

(defn cache-locally
  "Fetch an image to local file, return local path"
  [url]
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
               (cons "#wordpress")
               (clojure.string/join ", #"))))

(defn eligible-wp-post? [post]
  (log/debugf "Eligible-wp-post? %s" (ppr post))
  (and 
   (.contains (map clojure.string/lower-case (post :tags)) "freefeed")
   (not (data/seen-post? post))))

(defn eligible-ff-post? [post]
  (and
   ;;; Danger: take care of not reposting it back
   (.contains (clojure.string/lower-case (post :text)) "#ff2wp")
   (not (data/seen-post? post))))

(defn parse-wp-feed [xml-string]
  ;;(spit "/tmp/feed.xml" xml-string)
  (let [xml (xml-seq (xml/parse-str xml-string))]
    (for [item xml :when (= (get item :tag) :item)]
      (let [content (get item :content)
            tags (get-contents content :category)
            link (get-content content :link)
            post {:tags tags
                  :link link}]
        (when (eligible-wp-post? post)
          (let [post (assoc post :tags (map-tags tags))
                text (str (tools/html->text (get-content content :encoded))
                          "\n\n"
                          (footer post))]
            (log/debug "Text to be posted:" text)
            (merge post
                   {:imgs (get-post-images content :content)
                    :text text})))))))

(defn get-ff-images [post attachments]
  (remove nil?
          (map
           (fn [id]
             (when-let [url (get
                             (first (filter #(= (get % "id") id) attachments))
                             "url")]
               (cache-locally url)))
           (get post "attachments"))))
           
(defn parse-ff-feed [item]
  (let [body        (get item "posts")
        attachments (get item "attachments")]
    (for [item body]
      (let [post {:link (get item "id")
                  :text (get item "body")}]
        (when (eligible-ff-post? post)
          (assoc post :imgs (get-ff-images item attachments)))))))

(defn fetch-wp-poll []
  (log/info "Fetching Wordpress feed...")
  (let [res (http/get (CFG :wp-feed)
                      {:throw-exceptions false})]
    (if (= (res :status) 200)
      (let [entries (parse-wp-feed (res :body))
            new-entries (remove nil? entries)]
        (data/inc-state :wp_sessions)
        (data/logf :debug "Fetched %d entries: %d old, %d new"
                     (count entries)
                     (count (remove #(not (nil? %)) entries))
                     (count new-entries))
        new-entries)
      (do
        (data/inc-state :wp_errors)
        (data/logf :error "Cannot fetch WP feed: %s" (res :reason-phrase))
        nil))))

(defn cleanup [post]
  (run! io/delete-file (post :imgs)))

(defn configure []
  (let [wp-user (tools/env "WP_USER")
        wp-pass (tools/env "WP_PASS")
        ff-user (tools/env "FF_USER")
        ff-pass (tools/env "FF_PASS")
        port    (tools/env "PORT" 8080)
        wp-root (format "https://%s.wordpress.com/" wp-user)]
    (when (some nil? [wp-user wp-pass ff-user ff-pass])
      (log/error "Environment not set")
      (System/exit 1))
    (alter-var-root (var the-config)
                    #(merge %
                            {:wp-feed     (str wp-root "feed/")
                             :wp-api.root (str wp-root "/wp-json/wp")
                             :wp-user     wp-user
                             :wp-pass     wp-pass
                             :ff-user     ff-user
                             :ff-pass     ff-pass
                             :port        port}))))

(defn init-db []
  (data/init {:dbtype "postgresql"
              :dbname   (tools/env "DB_NAME" "wp2ff")
              :host     (tools/env "DB_HOST" "localhost")
              :user     (tools/env "DB_USER" "wp2ff")
              :password (tools/env "DB_PASS" "wp2ffpass")}))
  
(defn process-wp-poll [session post]
  (let [result (when-let [post-result (do-ff-post session post)]
                 (data/mark-seen post)
                 post-result)]
    (cleanup post)
    (assoc post :state result)))

(defn basic-auth-header [user pass]
  (->> (str user ":" pass)
       .getBytes
       #((Base64/getEncoder) %)
       (str "Basic ")))
  
(defn do-wp-post [post]
  (log/debugf "Posting new FF post to WP! Post: %s" (ppr post))
  (let [res (http/post (str (CFG :wp-api.root) "/v2/posts")
                       {:headers {"Authorization"
                                  (basic-auth-header (CFG :wp-user) (CFG :wp-pass))}
                        :throw-exceptions false
                        :content-type :json
                        :accept :json
                        :body (json/write-str {:title "Test message title"
                                               :status "draft"})})]
    (log/debugf "Post result: %s" (ppr res))
    nil))

(defn process-ff-poll [_ post]
  (let [result (when-let [post-result (do-wp-post post)]
                 (data/mark-seen post)
                 post-result)]
    (cleanup post)
    (assoc post :state result)))

(defn ff-session-error [err]
  (data/inc-state :ff_errors)
  (data/logf :error "FF login failed: %s" err)
  {:status 500
   :body "Cannot establish FreeFeed session"})

(defn process-feed [fetcher processor]
  (let [[session err] (create-ff-session)]
    (if session
      (do
        (data/inc-state :ff_sessions)
        (data/logf :debug "Created new FF session")
        (let [fetched-posts (fetcher)
              processed-posts (map #(processor session %) fetched-posts)
              failed-posts (remove #(get % :state) processed-posts)
              done-posts (vec (cset/difference (set processed-posts)
                                               (set failed-posts)))
              status (if (empty? failed-posts) (if (empty? done-posts) 200 201) 500)]
          {:status status
           :body (if (empty? processed-posts)
                   "No unseen posts yet"
                   (str
                    (when-not (empty? done-posts)
                      (format "Posted %d posts: %s\n" (count done-posts)
                              (clojure.string/join
                               ", " (map #(get % :link) done-posts))))
                        (when-not (empty? failed-posts)
                          (format "Failed %d posts: %s\n" (count failed-posts)
                                  (clojure.string/join
                                   ", " (map #(get % :link) failed-posts))))))}))
      (ff-session-error err))))

(defn fetch-ff-poll []
  (log/info "Fetching FF feed...")
  (let [res (http/get (format "%s/v2/timelines/%s" (CFG :api.root) (CFG :ff-user))
                      {:throw-exceptions false})]
    (if (= (res :status) 200)
      (let [entries (parse-ff-feed (json/read-str (res :body)))
            new-entries (remove nil? entries)]
        (data/inc-state :ff_sessions)
        (data/logf :debug "Fetched %d entries: %d old, %d new"
                   (count entries)
                   (count (remove #(not (nil? %)) entries))
                   (count new-entries))
        new-entries)
      (do
        (data/inc-state :ff_errors)
        (data/logf :error "Cannot fetch FF feed: %s" "(no diagnostics yet)")
        nil))))

(defn mresolve
  "No idea why (resolve sym) doesn't work"
  [sym]
  (ns-resolve (find-ns 'su.msk.xtalk.wp2ff.core) sym))

(defn process-feed-job
  "Called from GAE cron job"
  [job]
  (case job
    ("ff-poll" "wp-poll") (process-feed
                           (mresolve (symbol (format "fetch-%s"   job)))
                           (mresolve (symbol (format "process-%s" job))))
    {:status 400
     :body (format "Unknown job type: '%s'" job)}))

(defn -main [& args]
  (log/info "wp2ff v0.1, tobotras@gmail.com")
  (configure)
  (init-db)
  (log/debug "Set everything, serving")
  (service/start-web-service (the-config :port) process-feed-job))
