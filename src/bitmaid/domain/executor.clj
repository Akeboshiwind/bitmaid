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
            Method ProtectionCondition ForallExpression
            Operator AxiomPrecondition Axiom DomainExtension
            Problem]))

(def flatten-1 (partial mapcat identity))

(defprotocol JSHOPEncode
  (encode [this] "Encode the given form into a form that the JSHOP compiler will understand."))

(extend-protocol JSHOPEncode
  Variable
  (encode
    [{:keys [name]}]
    name)
  Constant
  (encode
    [{:keys [name]}]
    name)
  TermNumber
  (encode
    [{:keys [value]}]
    value)
  Call
  (encode
    [{:keys [function-symbol args]}]
    `(~'call ~function-symbol ~@(map encode args)))
  TermList
  (encode
    [{:keys [values]}]
    `(~@(map encode values)))
  Conjunction
  (encode
    [{:keys [expressions]}]
    (if (empty? expressions)
      `()
      `(~'and ~@(map encode expressions))))
  Disjunction
  (encode
    [{:keys [expressions]}]
    `(~'or ~@(map encode expressions)))
  Negation
  (encode
    [{:keys [expression]}]
    `(~'not ~(encode expression)))
  Implication
  (encode
    [{:keys [lhs rhs]}]
    `(~'imply ~(encode lhs) ~(encode rhs)))
  UniversalQuantification
  (encode
    [{:keys [variables predicate form]}]
    `(~'forall (~@variables)
               ~(encode predicate)
               ~(encode form)))
  Assignment
  (encode
    [{:keys [name value]}]
    `(~'assign ~name ~(encode value)))
  LateBinding
  (encode
    [{:keys [name]}]
    name)
  LogicalAtom
  (encode
    [{:keys [name args]}]
    `(~name ~@(map encode args)))
  FirstSatisfierPrecondition
  (encode
    [{:keys [expression]}]
    `(~':first ~(encode expression)))
  SortedPrecondition
  (encode
    [{:keys [variable comparator expression]}]
    `(~':sort-by ~variable ~comparator ~(encode expression)))
  TaskAtom
  (encode
    [{:keys [name args immediate? primitive?]}]
    `(~@(when immediate? [':immediate])
      ~name
      ~@(map encode args)))
  TaskList
  (encode
    [{:keys [task-lists ordered?]}]
    `(~@(when (not ordered?) [':unordered])
      ~@(map encode task-lists)))
  MethodOption
  (encode
    [{:keys [name precondition tail]}]
    `(~name
      ~(encode precondition)
      ~(encode tail)))
  Method
  (encode
    [{:keys [name args options]}]
    `(~':method (~name ~@args)
      ~@(flatten-1 (map encode options))))
  ProtectionCondition
  (encode
    [{:keys [atom]}]
    `(~':protection ~(encode atom)))
  ForallExpression
  (encode
    [{:keys [variables predicate logical-atoms]}]
    `(~'forall (~@variables)
      ~(encode predicate)
      (~@(map encode logical-atoms))))
  Operator
  (encode
    [{:keys [name args precondition delete-list add-list cost]}]
    `(~':operator (~name ~@args)
      ~(encode precondition)
      (~@(map encode delete-list))
      (~@(map encode add-list))
      ~cost))
  AxiomPrecondition
  (encode
    [{:keys [name precondition]}]
    `(~name
      ~(encode precondition)))
  Axiom
  (encode
    [{:keys [name args preconditions]}]
    `(~':- (~name ~@args)
      ~@(flatten-1 (map encode preconditions))))
  DomainExtension
  (encode
    [{:keys [axioms methods operators]}]
    `(~'defdomain ~'housedomain
      (~@(->> (list axioms methods operators)
              (map vals)
              (flatten-1)
              (map encode)))))
  Problem
  (encode
    [{:keys [initial-state task-list]}]
    `(~'defproblem ~'problem ~'housedomain
       (~@(map encode initial-state))
       (~@(encode task-list)))))
