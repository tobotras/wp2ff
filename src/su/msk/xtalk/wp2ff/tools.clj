(ns su.msk.xtalk.wp2ff.tools
  (:require [hickory.core :as hickory]
            [clojure.string :as str])
  (:gen-class))

;; CONVERT state to DB

(defn content [elt]
  "Element is a vector: tag, attributes, content..."
  (subvec elt 2))

(defn tag [elt]
  (first elt))

(defn dense-concat
  "Concatenate strings, adding space if there is no space between items"
  [strings]
  (loop [res ""
         s strings]
    (if (empty? s)
      res
      (let [cur (first s)
            tail (rest s)
            separator (if (or (empty? tail)
                              (str/blank? (subs cur (- (count cur) 1)))
                              (str/blank? (subs (first tail) 0 1)))
                        "" " ")]
        (recur (str res cur separator) tail)))))

(declare item->text)
(defn content->text [content]
  (dense-concat (map item->text content)))
  
(defn item->text [item]
  (if (vector? item)
    (let [cont (content item)
          tag (tag item)]
      (case tag
        :p (str (content->text cont) "\n")
        :em (str "/" (content->text cont) "/")
        :br "\n"
        (:figure) ""
        (str "ERR: unknown element <" tag ">")))
    (if (string? item)
      item
      (str "ERR: cannot parse " (type item)))))

(def ^:dynamic *max-text-size* (* 4 140))
 
(defn shorten
  "Pass first max-text-size chars, then cut on word end and add reference to link"
  [link s]
  (loop [i *max-text-size*]
    (if (> (inc i) (count s))
      s
      (if (clojure.string/blank? (subs s i (inc i)))
        (str (subs s 0 i) "... Read more in original post: " link)
        (recur (inc i))))))

(defn html->text [string link]
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
       (shorten link)))
  
(defn env [var & [default]]
  (if-let [value (System/getenv var)]
    (if (number? default)
      (Integer/parseUnsignedInt value)
      value)
    default))
