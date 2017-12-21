(ns advent.day-7
  (:use clojure.test))

(defrecord Program [name weight children])

(defn create-program-from-tokens [[name weight & children]]
  (Program. name (Integer/parseInt weight) children))

(defn tokenize-string [string]
  (clojure.string/split (clojure.string/replace string #"[\(\)\-\>#,]" "") #"\s+"))

(defn string-to-program [string]
  (->> string
       tokenize-string
       create-program-from-tokens))
  
(defn read-file-as-list-of-programs [filename]
  (->> filename
       (slurp)
       (clojure.string/split-lines)
       (map string-to-program)))

