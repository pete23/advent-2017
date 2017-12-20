(ns advent.day-12)

(defn string-to-set-of-numbers [string]
  (->>  (clojure.string/split (clojure.string/replace string #"[<\->,]" "") #"\s+")
        (map #(Integer/parseInt %))
        (set)))

(defn read-file-as-list-of-sets-of-numbers [filename]
  (->> filename
       (slurp)
       (clojure.string/split-lines)
       (map string-to-set-of-numbers)))

(defn intersect-groups [[group unmatched-groups] next-group]
  (if (not-empty (clojure.set/intersection group next-group))
    (vector (clojure.set/union group next-group) unmatched-groups)
    (vector group (conj unmatched-groups next-group))))
  
(defn group-until-fixed [group unmatched]
  (let [[new-group new-unmatched]
        (reduce intersect-groups [group []] unmatched)]
    (if (= group new-group)
      group 
      (recur new-group new-unmatched))))

(defn day-12-1 []
  (let [[zero-group & groups] (read-file-as-list-of-sets-of-numbers "day-12.input")]
    (count (group-until-fixed zero-group groups))))
  
