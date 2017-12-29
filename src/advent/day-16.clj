(ns advent.day-16
  (:use clojure.test))

(defn read-input-file [filename]
  (-> filename
      (slurp)
      (clojure.string/trim)
      (clojure.string/split #",")))

(def test-input ["s1" "x3/4" "pe/b"])

(defn generate-starting-position [size]
  (apply str (map #(char (+ % (int \a))) (range 0 size))))

(defn str-set-chr [s c i]
  (str (subs s 0 i) c (subs s (inc i))))

(deftest test-str-set-chr
  (is (= "abcY" (str-set-chr "abcd" \Y 3))
      (= "abYd" (str-set-chr "abcd" \Y 2))))

(defn spin [s size]
  (str (subs s (- (count s) size)) (subs s 0 (- (count s) size))))

(deftest test-spin
  (is (= "cdeab" (spin "abcde" 3))))

(defn exchange
  ([s inst]
   (let [[i j] (map #(Integer/parseInt %) (clojure.string/split inst #"/"))]
     (exchange s i j)))
  ([s i j]
   (-> s
       (str-set-chr (nth s i) j)
       (str-set-chr (nth s j) i))))

(deftest test-exchange
  (is (= "adcbe" (exchange "abcde" 1 3))
      (= "adcbe" (exchange "abcde" "1/3"))))

(defn partner
  ([^String s inst]
   (partner s (nth inst 0) (nth inst 2)))
  ([^String s a b]
   (exchange s (.indexOf s (int a)) (.indexOf s (int b)))))

(deftest test-partner
  (is (= "adcbe" (partner "abcde" \b \d))
      (= "adcbe" (partner "abcde" "b/d"))))

(defn run-instruction [dance-line instruction]
  (let [inst-fn (first instruction)
        args (subs instruction 1)]
    (case inst-fn
      \s (spin dance-line (Integer/parseInt args))
      \x (exchange dance-line args)
      \p (partner dance-line args))))

(defn run-dance
  ([dance-line instructions]
   (reduce run-instruction dance-line instructions))
  ([dance-line n instructions]
   (if (= n 0) dance-line
       (recur (run-dance dance-line instructions) (dec n) instructions))))

(defn part-1
  ([] (part-1 16 (read-input-file "input/day-16.txt")))
  ([size input] (-> size
                    (generate-starting-position)
                    (run-dance input))))

(deftest part-1-test
  (is (= "baedc" (part-1 5 test-input))))

(defn part-2
  ([] (part-2 16 (read-input-file "input/day-16.txt")))
  ([size input] (run-dance size 100 input))
  ([size n input] (-> size
                    (generate-starting-position)
                    (run-dance n input))))
