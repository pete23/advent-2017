(ns advent.day-18
  (:use clojure.test))

;; TODO use spec to represent this
;; program state
(def initial-state {:program-counter 0
                    :registers { :1 1 }
                    :sound-frequency nil
                    :recovered-frequency nil})

(defn numeric-string? [string]
  (re-matches #"\-?[0-9]+" string))

(defn translate-operand-to-fn [operand-string]
  (if (numeric-string? operand-string)
    ;; we have a numeric constant not state dependent
    (let [number (Integer/parseInt operand-string)]
      (fn [_] number))
    ;; we have a reference to a register
    (let [register (keyword operand-string)]
      (fn [state] (or (register (:registers state)) 0)))))

(defn reg-op-fn [op]
  (fn [state register register-value operand]
    (assoc-in state [:registers register] (op register-value operand))))

(defn jgz [state _ register-value operand]
  (if (> register-value 0)
    (assoc state :program-counter (+ (:program-counter state) operand -1)) ;; -1 corrects the default PC advance
    state))

(defn recover [state _ register-value _]
  (if (not= 0 register-value)
    (assoc state :recovered-frequency (:sound-frequency state))
    state))

(defn sound [state _ register-value _]
  (assoc state :sound-frequency register-value))

(defn create-op [op-fn register operand-fn]
  (fn [state]
    (let [register-value (or (register (:registers state) 0))
          operand (if operand-fn (operand-fn state))]
      (op-fn state register register-value operand))))

(defn plain-set [_ v] v)

(def instruction-set
  {"set" (reg-op-fn plain-set)
   "add" (reg-op-fn +)
   "mul" (reg-op-fn *)
   "mod" (reg-op-fn rem)
   "snd" sound
   "rcv" recover
   "jgz" jgz})

(defn create-instruction-from-strings [instruction-set [opcode register-string operand-string]]
  (let [register (keyword register-string)
        operand-fn (if operand-string (translate-operand-to-fn operand-string))]
    (create-op (instruction-set opcode) register operand-fn)))

(defn tokenize-string [string]
  (clojure.string/split string #"\s+"))

(defn string-to-instruction [instruction-set string]
  (->> string
       tokenize-string
       (create-instruction-from-strings instruction-set)))
  
(defn read-file-as-program [instruction-set filename]
  (->> filename
       (slurp)
       (clojure.string/split-lines)
       (mapv #(string-to-instruction instruction-set %))))

(defn take-step [program state]
;  (println state)
  (let [op (nth program (:program-counter state))]
    (-> state
        (op)
        (update-in [:program-counter] inc))))

(defn run-program
  ([program] (run-program program initial-state))
  ([program state] ;(println state)
   (if (:recovered-frequency state) state
         (recur program (take-step program state)))))

(def test-program (mapv #(string-to-instruction instruction-set %)
                        ["set a 1" "add a 2" "mul a a" "mod a 5"
                         "snd a" "set a 0" "rcv a" "jgz a -1"
                         "set a 1" "jgz a -2"]))

(deftest recover-4
  (is (= 4 (:recovered-frequency (run-program test-program)))))

(defn part-1 []
  (:recovered-frequency
   (run-program
    (read-file-as-program instruction-set "input/day-18.txt"))))

(defn send [state _ register-value _]
  (-> state
      (update-in [:outbound-queue] conj register-value)
      (update :outbound-sends inc)))

(defn receive [state register _ _]
  (if-let [message-received (peek (:inbound-queue state))]
    (-> state
        (assoc-in [:registers register] message-received)
        (update-in [:inbound-queue] pop))
    (-> state
        (assoc :blocking true)
        (update :program-counter dec))))
          
(def instruction-set-2
  (merge instruction-set
         {"snd" send
          "rcv" receive}))

(defn new-state [n]
  {:ident n
   :program-counter 0
   :registers { :p n :1 1 } ; set register 1 to avoid rewriting for values in position 1:-(
   :outbound-queue '()
   :outbound-sends 0
   :inbound-queue []
   :blocking false})

(defn run-program-twice
  ([program] (run-program-twice program (new-state 0) (new-state 1)))
  ([program active waiting]
   ;; deadlock if we are blocking, and come back into control with nothing on the queue
   (if (and (:blocking active) (empty? (:inbound-queue active)))
     (if (= 1 (:ident active)) active waiting) ; return program 1 state for the answer!
     (let [next-active (take-step program (assoc active :blocking false))]
       (if (:blocking next-active)
         (recur program
                (assoc waiting :inbound-queue (vec (:outbound-queue next-active)))
                (assoc next-active :outbound-queue '()))                
         (recur program next-active waiting))))))

(deftest example-part-2
  (let [program (mapv #(string-to-instruction instruction-set-2 %)
                      ["set x 7" "set y 2" "snd x" "snd y" "snd p" "rcv a" "rcv b" "rcv c" "rcv d"])
        end-state (run-program-twice program)
        end-registers (:registers end-state)]
    (is (= 3 (:outbound-sends end-state)))
    (is (= 7 (:a end-registers)))
    (is (= 2 (:b end-registers)))
    (is (= 1 (:c end-registers)))))

(defn part-2 []
  (:outbound-sends
   (run-program-twice
    (read-file-as-program instruction-set-2 "input/day-18.txt"))))
