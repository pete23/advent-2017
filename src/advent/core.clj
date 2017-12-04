(ns advent.core
  (:use clojure.math.numeric-tower))

; http://adventofcode.com/2017/day/1

(def advent-1-input "9513446799636685297929646689682997114316733445451534532351778534251427172168183621874641711534917291674333857423799375512628489423332297538215855176592633692631974822259161766238385922277893623911332569448978771948316155868781496698895492971356383996932885518732997624253678694279666572149831616312497994856288871586777793459926952491318336997159553714584541897294117487641872629796825583725975692264125865827534677223541484795877371955124463989228886498682421539667224963783616245646832154384756663251487668681425754536722827563651327524674183443696227523828832466473538347472991998913211857749878157579176457395375632995576569388455888156465451723693767887681392547189273391948632726499868313747261828186732986628365773728583387184112323696592536446536231376615949825166773536471531487969852535699774113163667286537193767515119362865141925612849443983484245268194842563154567638354645735331855896155142741664246715666899824364722914296492444672653852387389477634257768229772399416521198625393426443499223611843766134883441223328256883497423324753229392393974622181429913535973327323952241674979677481518733692544535323219895684629719868384266425386835539719237716339198485163916562434854579365958111931354576991558771236977242668756782139961638347251644828724786827751748399123668854393894787851872256667336215726674348886747128237416273154988619267824361227888751562445622387695218161341884756795223464751862965655559143779425283154533252573949165492138175581615176611845489857169132936848668646319955661492488428427435269169173654812114842568381636982389224236455633316898178163297452453296667661849622174541778669494388167451186352488555379581934999276412919598411422973399319799937518713422398874326665375216437246445791623283898584648278989674418242112957668397484671119761553847275799873495363759266296477844157237423239163559391553961176475377151369399646747881452252547741718734949967752564774161341784833521492494243662658471121369649641815562327698395293573991648351369767162642763475561544795982183714447737149239846151871434656618825566387329765118727515699213962477996399781652131918996434125559698427945714572488376342126989157872118279163127742349")

(defn char-to-digit [char]
  (- (int char) (int \0)))

(defn string-to-digits [string]
  (map char-to-digit string))

; pair each digit with the next digit - add the last digit to the start of the sequence to wrap around
(defn advent-1-1-pairing-fn [digits]
  (partition 2 1 (conj digits (last digits))))

; split the digits into two equal sized lists and pair up each element
(defn advent-1-2-pairing-fn [digits]
  (apply map #(vector %1 %2) (split-at (/ (count digits) 2) digits)))

; create pairs from a string of numbers, then add the numbers where the pairs match
(defn advent-1-engine [string pairing-fn]
  (let [digits (string-to-digits string)
        pairs (pairing-fn digits)]
    (reduce + (map first (filter #(apply = %) pairs)))))

(defn advent-1-1 [string]
  (advent-1-engine string advent-1-1-pairing-fn))

(defn advent-1-2 [string]
  ; need to be x2 as our pairing engine only pairs the first half with the second not vice versa
  ; due to the symmetry we can just x2 rather than do twice the work...
  (* 2 (advent-1-engine string advent-1-2-pairing-fn)))

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
       (manhattan-distance-from-origin)))

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

(def advent-4-input (->> "advent-4-input.txt"
                         (slurp)
                         (clojure.string/split-lines)
                         (map #(clojure.string/split % #"\s"))))

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

