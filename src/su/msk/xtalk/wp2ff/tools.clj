(ns su.msk.xtalk.wp2ff.tools
  (:require [hickory.core :as hickory])
  (:gen-class))

;; CONVERT state to DB

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

(def MAX_TEXT_SIZE (* 4 140))
 
(defn shorten
  "Pass first MAX_TEXT_SIZE chars, then cut on word end and add reference to link"
  [s]
  (if (< (count s) MAX_TEXT_SIZE)
    s
    (loop [i MAX_TEXT_SIZE]
      (if (> (inc i) (count s))
        s
        (if (clojure.string/blank? (subs s i (inc i)))
          (str (subs s 0 i) "... (Read more in original post)")
          (recur (inc i)))))))

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
       (clojure.string/join "")
       shorten))
  
(defn env [var & [default]]
  (if-let [value (System/getenv var)]
    (if (number? default)
      (Integer/parseUnsignedInt value)
      value)
    default))
