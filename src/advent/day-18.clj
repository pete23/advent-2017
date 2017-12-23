(ns advent.day-18
  (:use clojure.test))

;; TODO use spec to represent this
;; program state
(def initial-state {:program-counter 0
                    :registers {}
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

(defn create-instruction-from-strings [[opcode register-string operand-string]]
  (let [register (keyword register-string)
        operand-fn (if operand-string (translate-operand-to-fn operand-string))]
    (create-op (instruction-set opcode) register operand-fn)))

(defn tokenize-string [string]
  (clojure.string/split string #"\s+"))

(defn string-to-instruction [string]
  (->> string
       tokenize-string
       create-instruction-from-strings))
  
(defn read-file-as-program [filename]
  (->> filename
       (slurp)
       (clojure.string/split-lines)
       (mapv string-to-instruction)))


(defn run-program
  ([program] (run-program program initial-state))
  ([program state] ;(println state)
   (if (:recovered-frequency state) state
       (let [new-state (-> state
                           ((nth program (:program-counter state)))
                           (update-in [:program-counter] inc))]
         (recur program new-state)))))

(def test-program (mapv string-to-instruction ["set a 1" "add a 2" "mul a a" "mod a 5"
                                               "snd a" "set a 0" "rcv a" "jgz a -1"
                                               "set a 1" "jgz a -2"]))

(deftest recover-4
  (is (= 4 (:recovered-frequency (run-program test-program)))))

(defn part-1 []
  (:recovered-frequency (run-program (read-file-as-program "input/day-18.txt"))))
