(ns bitmaid.domain.executor
  (:require [bitmaid.domain.compiler :as c]
            [bitmaid.domain.parser :as p]
            [cljs.spec.alpha :as s]))

(def flatten-1 (partial mapcat identity))

(defprotocol JSHOPCompile
  (compile [this] "Compile the given form into a form that the JSHOP compiler will understand."))

(extend-protocol JSHOPCompile
  c/Variable
  (compile
    [{:keys [name]}]
    name)
  c/Constant
  (compile
    [{:keys [name]}]
    name)
  c/Number
  (compile
    [{:keys [value]}]
    value)
  c/Call
  (compile
    [{:keys [function-symbol args]}]
    `(~'call ~function-symbol ~@(map compile args)))
  c/TermList
  (compile
    [{:keys [values]}]
    `(~@(map compile values)))
  c/Conjunction
  (compile
    [{:keys [expressions]}]
    (if (empty? expressions)
      `()
      `(~'and ~@(map compile expressions))))
  c/Disjunction
  (compile
    [{:keys [expressions]}]
    `(~'or ~@(map compile expressions)))
  c/Negation
  (compile
    [{:keys [expression]}]
    `(~'not ~(compile expression)))
  c/Implication
  (compile
    [{:keys [lhs rhs]}]
    `(~'imply ~(compile lhs) ~(compile rhs)))
  c/UniversalQuantification
  (compile
    [{:keys [variables predicate form]}]
    `(~'forall (~@variables)
               ~(compile predicate)
               ~(compile form)))
  c/Assignment
  (compile
    [{:keys [name value]}]
    `(~'assign ~name ~(compile value)))
  c/LateBinding
  (compile
    [{:keys [name]}]
    name)
  c/LogicalAtom
  (compile
    [{:keys [name args]}]
    `(~name ~@(map compile args)))
  c/FirstSatisfierPrecondition
  (compile
    [{:keys [expression]}]
    `(~':first ~(compile expression)))
  c/SortedPrecondition
  (compile
    [{:keys [variable comparator expression]}]
    `(~':sort-by ~variable ~comparator ~(compile expression)))
  c/TaskAtom
  (compile
    [{:keys [name args immediate? primitive?]}]
    `(~@(when immediate? [':immediate])
      ~name
      ~@(map compile args)))
  c/TaskList
  (compile
    [{:keys [task-lists ordered?]}]
    `(~@(when (not ordered?) [':unordered])
      ~@(map compile task-lists)))
  c/MethodOption
  (compile
    [{:keys [name precondition tail]}]
    `(~name
      ~(compile precondition)
      ~(compile tail)))
  c/Method
  (compile
    [{:keys [name args options]}]
    `(~':method (~name ~@args)
      ~@(flatten-1 (map compile options))))
  c/ProtectionCondition
  (compile
    [{:keys [atom]}]
    `(~':protection ~(compile atom)))
  c/Operator
  (compile
    [{:keys [name args precondition delete-list add-list cost]}]
    `(~':operator (~name ~@args)
      ~(compile precondition)
      (~@(map compile delete-list))
      (~@(map compile add-list))
      ~cost))
  c/AxiomPrecondition
  (compile
    [{:keys [name precondition]}]
    `(~name
      ~(compile precondition)))
  c/Axiom
  (compile
    [{:keys [name args preconditions]}]
    `(~':- (~name ~@args)
      ~@(flatten-1 (map compile preconditions))))
  c/DomainExtension
  (compile
    [{:keys [axioms methods operators]}]
    `(~'defdomain ~'housedomain
      (~@(->> (list axioms methods operators)
              (map vals)
              (flatten-1)
              (map compile)))))
  c/Problem
  (compile
    [{:keys [name initial-state task-list]}]
    `(~'defproblem ~name ~'housedomain
       (~@(compile initial-state))
       (~@(compile task-list)))))
