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

(def day-7-test (map string-to-program
                     ["pbga (66)"
                      "xhth (57)"
                      "ebii (61)"
                      "havc (66)"
                      "ktlj (57)"
                      "fwft (72) -> ktlj, cntj, xhth"
                      "qoyq (66)"
                      "padx (45) -> pbga, havc, qoyq"
                      "tknk (41) -> ugml, padx, fwft"
                      "jptl (61)"
                      "ugml (68) -> gyxo, ebii, jptl"
                      "gyxo (61)"
                      "cntj (57)"]))

; the one at the base cannot be a child
(defn find-base [programs]
  (let [all-children (set (mapcat :children programs))]
      (some #(if (not (all-children %)) %) (map :name programs))))

(deftest find-base-works
  (is (= "tknk" (find-base day-7-test))))

(defn part-1 []
  (find-base (read-file-as-list-of-programs "input/day-7.txt")))

