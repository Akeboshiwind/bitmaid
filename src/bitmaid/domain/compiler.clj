(ns bitmaid.domain.compiler
  (:require [bitmaid.domain.parser :as p]
            [clojure.spec.alpha :as s]))

;; Utils
(def gen-debug-name gensym)
(defn list-args
  "Given a spec form, return a vector of the variables found."
  [spec-form]
  (->> (p/walk-tree spec-form)
       (filter #(and (seqable? %)
                     (= :variable-symbol (first %))))
       (map second)))

;; Compiler
(defrecord Variable [name])
(defrecord Constant [name])
(defrecord TermNumber [value])

(defrecord Call [function-symbol args])
(def compile-term nil)
(defn compile-call
  [call]
  (map->Call {:function-symbol (:function-symbol call)
              :args (map compile-term (:terms call))}))


(defrecord TermList [values])
(defn compile-list
  [list]
  (map->TermList {:values (map compile-term list)}))

(defn compile-term
  [term]
  (let [type (first term)
        body (second term)]
    (case type
      :variable-symbol (map->Variable {:name body})
      :constant-symbol (map->Constant {:name body})
      :number (map->TermNumber {:value body})
      :call (compile-call body)
      :list (compile-list body))))

(def compile-expression nil)
(defrecord Conjunction [expressions])
(defn compile-conjunction
  [con]
  (let [empty? (= :empty (first con))
        body (second con)
        expressions (if empty?
                      []
                      (:expressions body))]
    (map->Conjunction {:expressions (map compile-expression expressions)})))

(defrecord Disjunction [expressions])
(defn compile-disjunction
  [dis]
  (map->Disjunction {:expressions (map compile-expression (:expressions dis))}))

(defrecord Negation [expression])
(defn compile-negation
  [neg]
  (map->Negation {:expression (compile-expression (:expression neg))}))

(defrecord Implication [lhs rhs])
(defn compile-implication
  [implication]
  (map->Implication {:lhs (compile-expression (:lhs implication))
                     :rhs (compile-expression (:rhs implication))}))

(defrecord UniversalQuantification [variables predicate form])
(defn compile-universal-quantification
  [quantif]
  (map->UniversalQuantification {:variables (:variable-symbols quantif)
                                 :predicate (compile-expression (:predicate quantif))
                                 :form (compile-expression (:form quantif))}))

(defrecord Assignment [name value])
(defn compile-assignment
  [assignment]
  (map->Assignment {:name (:name assignment)
                    :value (compile-term (:value assignment))}))

(defrecord LateBinding [name])
(defrecord LogicalAtom [name args])
(defn compile-base-logical-atom
  [atom]
  (let [name (:predicate-symbol atom)]
    (map->LogicalAtom {:name name
                       :args (map compile-term (:terms atom))})))
(defn compile-logical-atom
  [atom]
  (let [late-binding? (= :late-binding (first atom))
        body (second atom)]
    (if late-binding?
      (map->LateBinding {:name body})
      (compile-base-logical-atom body))))

(defn compile-expression
  [expression]
  (let [type (first expression)
        body (second expression)]
    (case type
      :conjunction  (compile-conjunction body)
      :disjunction  (compile-disjunction body)
      :negation     (compile-negation body)
      :implication  (compile-implication body)
      :universal-quantification (compile-universal-quantification body)
      :assignment   (compile-assignment body)
      :call         (compile-call body)
      :logical-atom (compile-logical-atom body))))

(defrecord FirstSatisfierPrecondition [expression])
(defn compile-first-satisfier-precondition
  [precond]
  (map->FirstSatisfierPrecondition {:expression (compile-expression (:expression precond))}))

(defrecord SortedPrecondition [variable comparator expression])
(defn compile-sorted-precondition
  [precond]
  (map->SortedPrecondition {:variable (:variable-symbol precond)
                            :comparator (:comparator precond)
                            :expression (compile-expression (:expression precond))}))

(defn compile-logical-precondition
  [precond]
  (let [type (first precond)
        body (second precond)]
    (case type
      :first-satisfier-precondition (compile-first-satisfier-precondition body)
      :sorted-precondition (compile-sorted-precondition body)
      :expression (compile-expression body))))

(defrecord TaskAtom [name args immediate? primitive?])
(defn compile-task-atom
  [atom]
  (let [immediate? (= :immediate-task (first atom))
        body (if immediate?
               (:task (second atom))
               (second atom))
        primitive? (= :primitive (first body))
        body (second body)
        name (:name body)
        args (map compile-term (:terms body))]
    (map->TaskAtom {:name name
                    :args args
                    :immediate? immediate?
                    :primitive? primitive?})))

(defrecord TaskList [task-lists ordered?])
(def compile-task-list nil)
(defn compile-task-list*
  [list]
  (let [task-atom? (= :task-atom (first list))
        body (second list)]
    (if task-atom?
      (compile-task-atom body)
      (compile-task-list body))))

(defn compile-task-list
  [list]
  (let [ordered? (= :ordered (first list))
        body (second list)]
    (map->TaskList {:task-lists (map compile-task-list* (:task-lists body))
                    :ordered? ordered?})))

(defrecord MethodOption [name precondition tail])
(defn compile-method-option
  [option]
  (let [named? (= :named (first option))
        body (second option)
        name (if named?
               (:name body)
               (gen-debug-name "method-branch_"))
        precondition (compile-logical-precondition (:precondition body))
        tail (compile-task-list (:tail body))]
    (map->MethodOption {:name name
                        :precondition precondition
                        :tail tail})))

(defrecord Method [name args options])
(defn method-name
  [method]
  (-> method
      :head
      second
      :name))

(defn compile-method
  [method]
  (let [name (method-name method)
        args (list-args (:head method))
        options (map compile-method-option (:options method))]
    (map->Method {:name name
                  :args args
                  :options options})))

(defrecord ProtectionCondition [atom])
(defn compile-protection-condition
  [protec]
  (map->ProtectionCondition {:atom (compile-logical-atom (:logical-atom protec))}))

(defrecord ForallExpression [variables predicate logical-atoms])
(defn compile-forall-expression
  [forall]
  (map->ForallExpression {:variables (:variable-symbols forall)
                          :predicate (compile-expression (:predicate forall))
                          :form (map compile-logical-atom (:logical-atoms forall))}))

(defn compile-delete-add-element
  [element]
  (let [type (first element)
        body (second element)]
    (case type
      :logical-atom (compile-logical-atom body)
      :protection-condition (compile-protection-condition body)
      :forall-expresssion (compile-forall-expression body))))

(defn compile-delete-add-list
  [list]
  (let [late-binding? (= :late-binding (first list))
        body (second list)]
    (if late-binding?
      (map->LateBinding {:name body})
      (map compile-delete-add-element body))))

(defn operator-name
  [operator]
  (-> operator
      second
      :head
      second
      :name))

(defrecord Operator [name args precondition delete-list add-list cost])
(defn compile-operator
  [operator]
  (let [has-cost? (= :set-cost (first operator))
        body (second operator)
        cost (if has-cost?
               (:cost body)
               1)
        name (operator-name operator)
        args (list-args (:head body))
        precondition (compile-logical-precondition (:precondition body))
        delete-list (compile-delete-add-list (:delete-list body))
        add-list (compile-delete-add-list (:add-list body))]
    (map->Operator {:name name
                    :args args
                    :precondition precondition
                    :delete-list delete-list
                    :add-list add-list
                    :cost cost})))

(defrecord AxiomPrecondition [name precondition])
(defn compile-axiom-precondition
  [precondition]
  (let [named? (= :named (first precondition))
        body (second precondition)
        name (if named?
               (:name body)
               (gen-debug-name "axiom-precondition_"))]
    (map->AxiomPrecondition {:name name
                             :precondition (compile-logical-precondition (:logical-precondition body))})))

(defn axiom-name
  [axiom]
  (-> axiom
      :head
      second
      :predicate-symbol))

(defrecord Axiom [name args preconditions])
(defn compile-axiom
  [axiom]
  (map->Axiom {:name (axiom-name axiom)
               :args (list-args (:head axiom))
               :preconditions (map compile-axiom-precondition (:axioms axiom))}))


(defrecord DomainExtension [methods operators axioms])
(defn compile-domain-extension
  [domain-extension]
  (loop [acc domain-extension
         methods {}
         operators {}
         axioms {}]
    (if (empty? acc)
      (map->DomainExtension {:methods methods
                             :operators operators
                             :axioms axioms})
      (let [next (first acc)
            type (first next)
            body (second next)]
        (case type
          :method (recur (rest acc)
                         (assoc methods (method-name body) (compile-method body))
                         operators
                         axioms)
          :operator (recur (rest acc)
                           methods
                           (assoc operators (operator-name body) (compile-operator body))
                           axioms)
          :axiom (recur (rest acc)
                        methods
                        operators
                        (assoc axioms (axiom-name body) (compile-axiom body))))))))

(defrecord Problem [initial-state task-list])
(defn compile-problem
  [problem]
  (let [initial-state (map compile-base-logical-atom (:initial-state problem))
        task-list (compile-task-list (:task-list problem))]
    (map->Problem {:initial-state initial-state
                   :task-list task-list})))

(defn precompile-domain-extension
  [domain-extension]
  (compile-domain-extension
   (p/parse-domain-extension domain-extension)))

(defn precompile-problem
  [problem]
  (compile-problem
   (p/parse-problem problem)))
