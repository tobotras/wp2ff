(ns su.msk.xtalk.wp2ff.tools-test
  (:require [clojure.test :refer :all]
            [su.msk.xtalk.wp2ff.tools :refer :all]))

(deftest t-shorten
  (binding [*max-text-size* 5]
    (are [x y] (= x y)
      (shorten "1234") "1234"
      (shorten "12345") "12345"
      (shorten "123456") "123456"
      (shorten "12 45") "12 45"
      (shorten "12 34\n56") "12 34... (Read more in original post)")))

(deftest t-html->text
  (is (= (html->text "<html><body><p>One, <em>and</em><p>Two<br>and three")
         "One, /and/\nTwo\nand three\n")))
