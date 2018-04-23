(ns bitmaid.domain.executor
  (:require [bitmaid.domain.compiler :as c]))

(defprotocol Consequent
  (consequent? [this state axiom-list] "Return true if `this` is consequent of `state` and `axiom-list`."))

(extend-protocol Consequent
  c/LogicalAtom
  (consequent?
    [this state axiom-list]
    ()))

(defn consequent?
  "Returns true if `l` is consequent of state `s` and axiom list `x`."
  [l s x]
  (case (:type l)
    :logical-atom))

(defn exec
  [domain state]
  (let [{:keys [axioms operators methods]} domain]
    ()))

(defn solved?
  [{:keys [state plan protections domain]}]
  (or (empty? plan)
      (every? simple-task? plan)))

(defn plan
  "Given a domain and problem description, work out a list of tasks to execute."
  [domain problem]
  (loop [state {:state (:initial-state problem)
                :plan (:task-list problem)
                :protections []
                :domain domain}]
    (if (solved? state)
      plan
      (recur))))
