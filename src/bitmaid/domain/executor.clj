(ns bitmaid.domain.executor
  (:require [bitmaid.domain.compiler :as c]
            [bitmaid.domain.parser :as p]
            [clojure.spec.alpha :as s])
  ;; Import records
  (:import [bitmaid.domain.compiler Variable
            Constant TermNumber Call TermList
            Conjunction Disjunction Negation
            Implication UniversalQuantification
            Assignment LateBinding LogicalAtom
            FirstSatisfierPrecondition SortedPrecondition
            TaskAtom TaskList MethodOption
            Method ProtectionCondition Operator
            AxiomPrecondition Axiom DomainExtension
            Problem]))

(def flatten-1 (partial mapcat identity))

(defprotocol JSHOPCompile
  (compile [this] "Compile the given form into a form that the JSHOP compiler will understand."))

(extend-protocol JSHOPCompile
  Variable
  (compile
    [{:keys [name]}]
    name)
  Constant
  (compile
    [{:keys [name]}]
    name)
  TermNumber
  (compile
    [{:keys [value]}]
    value)
  Call
  (compile
    [{:keys [function-symbol args]}]
    `(~'call ~function-symbol ~@(map compile args)))
  TermList
  (compile
    [{:keys [values]}]
    `(~@(map compile values)))
  Conjunction
  (compile
    [{:keys [expressions]}]
    (if (empty? expressions)
      `()
      `(~'and ~@(map compile expressions))))
  Disjunction
  (compile
    [{:keys [expressions]}]
    `(~'or ~@(map compile expressions)))
  Negation
  (compile
    [{:keys [expression]}]
    `(~'not ~(compile expression)))
  Implication
  (compile
    [{:keys [lhs rhs]}]
    `(~'imply ~(compile lhs) ~(compile rhs)))
  UniversalQuantification
  (compile
    [{:keys [variables predicate form]}]
    `(~'forall (~@variables)
               ~(compile predicate)
               ~(compile form)))
  Assignment
  (compile
    [{:keys [name value]}]
    `(~'assign ~name ~(compile value)))
  LateBinding
  (compile
    [{:keys [name]}]
    name)
  LogicalAtom
  (compile
    [{:keys [name args]}]
    `(~name ~@(map compile args)))
  FirstSatisfierPrecondition
  (compile
    [{:keys [expression]}]
    `(~':first ~(compile expression)))
  SortedPrecondition
  (compile
    [{:keys [variable comparator expression]}]
    `(~':sort-by ~variable ~comparator ~(compile expression)))
  TaskAtom
  (compile
    [{:keys [name args immediate? primitive?]}]
    `(~@(when immediate? [':immediate])
      ~name
      ~@(map compile args)))
  TaskList
  (compile
    [{:keys [task-lists ordered?]}]
    `(~@(when (not ordered?) [':unordered])
      ~@(map compile task-lists)))
  MethodOption
  (compile
    [{:keys [name precondition tail]}]
    `(~name
      ~(compile precondition)
      ~(compile tail)))
  Method
  (compile
    [{:keys [name args options]}]
    `(~':method (~name ~@args)
      ~@(flatten-1 (map compile options))))
  ProtectionCondition
  (compile
    [{:keys [atom]}]
    `(~':protection ~(compile atom)))
  Operator
  (compile
    [{:keys [name args precondition delete-list add-list cost]}]
    `(~':operator (~name ~@args)
      ~(compile precondition)
      (~@(map compile delete-list))
      (~@(map compile add-list))
      ~cost))
  AxiomPrecondition
  (compile
    [{:keys [name precondition]}]
    `(~name
      ~(compile precondition)))
  Axiom
  (compile
    [{:keys [name args preconditions]}]
    `(~':- (~name ~@args)
      ~@(flatten-1 (map compile preconditions))))
  DomainExtension
  (compile
    [{:keys [axioms methods operators]}]
    `(~'defdomain ~'housedomain
      (~@(->> (list axioms methods operators)
              (map vals)
              (flatten-1)
              (map compile)))))
  Problem
  (compile
    [{:keys [name initial-state task-list]}]
    `(~'defproblem ~name ~'housedomain
       (~@(map compile initial-state))
       (~@(compile task-list)))))
