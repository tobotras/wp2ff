(ns su.msk.xtalk.wp2ff.tools
  (:require [hickory.core :as hickory])
  (:gen-class))

(defn content [elt]
  "Element is a vector: tag, attributes, content..."
  (subvec elt 2))

(defn tag [elt]
  (first elt))

(declare item->text)
(defn content->text [content]
  (clojure.string/join " " (mapv item->text content)))
  
(defn item->text [item]
  (println "item->text got item" item)
  (if (vector? item)
    (let [cont (content item)
          tag (tag item)]
      (case tag
        :p (content->text cont)
        :em (str "/" (content->text cont) "/")
        :br "\n"
        (:figure) ""
        (str "ERR: unknown element <" tag ">")))
    (if (string? item)
      item
      (str "ERR: cannot parse " (type item)))))

(defn html->text [string]
  (->> string
       hickory/parse
       hickory/as-hiccup
       first
       content
       (filter #(= (tag %) :body))
       (remove nil?)
       first
       content
       (mapv item->text)
       (clojure.string/join "")))
  
