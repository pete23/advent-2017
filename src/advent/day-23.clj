(ns advent.day-23
  (:use clojure.test))

(defn counted-mul [state register register-value operand]
  (-> state
      (assoc-in [:registers register] (* register-value operand))
      (update-in [:mul-count] inc)))

(def part-1-instruction-set (merge
                             (advent.register-machine/make-instructions)
                             { "mul" counted-mul }))

(def part-1-initial-state (merge
                           (advent.register-machine/make-state)
                           { :mul-count 0 }))

(defn pc-out-of-bounds? [program {:keys [program-counter]}]
  (or (> 0 program-counter) (<= (count program) program-counter)))
  
(defn run-program [program state]
  (if (pc-out-of-bounds? program state) state
      (recur program (advent.register-machine/take-step program state))))

