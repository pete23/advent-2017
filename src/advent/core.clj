(ns advent.core
  (:use clojure.math.numeric-tower))

; http://adventofcode.com/2017/day/2

(def advent-2-input [
                     [5806	6444	1281	38	267	1835	223	4912	5995	230	4395	2986	6048	4719	216	1201]
                     [74	127	226	84	174	280	94	159	198	305	124	106	205	99	177	294]
                     [1332	52	54	655	56	170	843	707	1273	1163	89	23	43	1300	1383	1229]
                     [5653	236	1944	3807	5356	246	222	1999	4872	206	5265	5397	5220	5538	286	917]
                     [3512	3132	2826	3664	2814	549	3408	3384	142	120	160	114	1395	2074	1816	2357]
                     [100	2000	112	103	2122	113	92	522	1650	929	1281	2286	2259	1068	1089	651]
                     [646	490	297	60	424	234	48	491	245	523	229	189	174	627	441	598]
                     [2321	555	2413	2378	157	27	194	2512	117	140	2287	277	2635	1374	1496	1698]
                     [101	1177	104	89	542	2033	1724	1197	474	1041	1803	770	87	1869	1183	553]
                     [1393	92	105	1395	1000	85	391	1360	1529	1367	1063	688	642	102	999	638]
                     [4627	223	188	5529	2406	4980	2384	2024	4610	279	249	2331	4660	4350	3264	242]
                     [769	779	502	75	1105	53	55	931	1056	1195	65	292	1234	1164	678	1032]
                     [2554	75	4406	484	2285	226	5666	245	4972	3739	5185	1543	230	236	3621	5387]
                     [826	4028	4274	163	5303	4610	145	5779	157	4994	5053	186	5060	3082	2186	4882]
                     [588	345	67	286	743	54	802	776	29	44	107	63	303	372	41	810]
                     [128	2088	3422	111	3312	740	3024	1946	920	131	112	477	3386	2392	1108	2741]])

(defn max-min [acc next]
  (vector (max (first acc) next)
          (min (second acc) next)))

(defn advent-2-1 [input]
  (reduce + (map #(apply - (reduce max-min [Integer/MIN_VALUE Integer/MAX_VALUE] %)) input)))

(defn evenly-divides-but-is-not [numerator denominator]
  (if (not (= numerator denominator))
    (if (= 0 (mod numerator denominator))
      (/ numerator denominator))))

(defn even-division [numerator denominators]
  (some #(evenly-divides-but-is-not numerator %) denominators))

(defn first-even-division [row]
  (some #(even-division % row) row))

(defn advent-2-2 [input]
  (reduce + (map first-even-division input)))

;; day 3 - this sucks - there's an analytic formula for spiral number to coordinates
;; but unfortunately my discrete maths has decayed and it was easier just to generate the spiral

(defn manhattan-distance-to-origin [[x y]]
  (+ (abs x) (abs y)))

(defn add-coords [[a b] [c d]]
  (vector (+ a c) (+ b d)))

(defn next-arm [step prev]
  (let [chains (if (odd? step)
                 [[1 0] [0 1]]
                 [[-1 0] [0 -1]])]
    (drop 1 (reductions add-coords prev (mapcat #(repeat step %) chains)))))

(defn lazy-seq-of-spiral-cartesians
  ([] (cons [0 0] (lazy-seq (lazy-seq-of-spiral-cartesians 1 '(0 0)))))
  ([step prev] (let [arm (next-arm step prev)]
              (concat arm (lazy-seq (lazy-seq-of-spiral-cartesians (inc step) (last arm)))))))

(def spiral (lazy-seq-of-spiral-cartesians))

(defn advent-3-1 [spiral-number]
  (->> spiral-number
       (dec) ; correct their coordinate system
       (nth spiral)
       (manhattan-distance-to-origin)))

(defn adjacent-sums [grid coords]
  (reduce + (remove nil? (map #(get grid (add-coords coords %)) [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 1] [1 0] [1 -1]]))))

(defn add-adjacent-sum [[_ grid] next-coords]
  (let [next-value (adjacent-sums grid next-coords)]
    (vector next-value (assoc grid next-coords next-value))))
    
(defn advent-3-2 [spiral-number]
  (some #(if (< spiral-number (first %)) (first %))
        (reductions add-adjacent-sum [1 { '(0 0) 1 }] (drop 1 spiral))))

(advent-3-2 368078)

;; https://adventofcode.com/2017/day/4

(defn read-file-as-list-of-lists [filename]
  (->> filename
       (slurp)
       (clojure.string/split-lines)
       (map #(clojure.string/split % #"\s"))))
  
(def advent-4-input
  (read-file-as-list-of-lists "advent-4-input.txt"))

;; if we stuff stuff into a set and the set has the same cardinality as the stuff
;; then there are no duplicates in the stuff
(defn check-no-duplicates [stuff]
  (if (= (count stuff)
         (count (into #{} stuff)))
    1 0))

(defn advent-4-1 []
  (reduce + (map check-no-duplicates advent-4-input)))

(defn advent-4-2 []
  (reduce + (map check-no-duplicates (map #(map sort %) advent-4-input))))

;; advent day 8

(defn safe-op-map [m]
  (into {} (map #(vector (first %) (fnil (second %) 0 0)) m)))

(def string-to-op-map
  (safe-op-map
   {"inc" +
    "dec" - }))

(def string-to-predicate-map
  (safe-op-map
   {"!=" not=
    "==" =
    "<" <
    ">" >
    "<=" <=
    ">=" >=}))


(defn advent-8-parse-instruction 
  ;; b inc 5 if a > 1
  [[reg op-str operand-str _ pred-reg pred-str pred-operand-str]]
  {:reg reg
   :op (string-to-op-map op-str)
   :operand (Integer/parseInt operand-str)
   :pred-reg pred-reg
   :pred (string-to-predicate-map pred-str)
   :pred-operand (Integer/parseInt pred-operand-str)})
   
(defn advent-8-execute-instruction [state
                                    {:keys [reg op operand pred-reg pred pred-operand]}]
  (let [[current-max registers] state]
    (if (pred (registers pred-reg) pred-operand)
      (let [new-value (op (registers reg) operand)
            new-max (max current-max new-value)]
        (vector new-max (assoc registers reg new-value)))
        [current-max registers])))
  
(defn advent-8-engine [input]
  (->> input
       (map advent-8-parse-instruction)
       (reduce advent-8-execute-instruction [0 {}])))

(defn advent-8-1-test []
  (->> [["b" "inc" "5" "if" "a" ">" "1"]
        ["a" "inc" "1" "if" "b" "<" "5"]
        ["c" "dec" "-10" "if" "a" ">=" "1"]
        ["c" "inc" "-20" "if" "c" "==" "10"]]
       (advent-8-engine)))
         
(defn advent-8-1 []
  (->> "advent-8-input.txt"
       (read-file-as-list-of-lists)
       (advent-8-engine)
       (second)
       (map second)
       (reduce max)))

(defn advent-8-2 []
  (->> "advent-8-input.txt"
       (read-file-as-list-of-lists)
       (advent-8-engine)
       (first)))

