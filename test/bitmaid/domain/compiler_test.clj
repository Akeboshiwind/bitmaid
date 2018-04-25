(ns bitmaid.domain.compiler-test
  (:require [bitmaid.domain.compiler :as c]
            [bitmaid.domain.parser :as p]
            [clojure.spec.alpha :as s]
            [clojure.test :as t :include-macros true :refer [deftest testing is]])
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

(deftest test-utils
  (testing "gen-debug-name"
    (is (symbol? (c/gen-debug-name))))
  (testing "list-args"
    (is (= [1 2 3]
           (c/list-args [[:variable-symbol 1] {:test [[:variable-symbol 2]]
                                               :another {:yet-again [[:variable-symbol 3]]}}])))))

(deftest test-term
  (testing "compile-call"
    (let [term (s/conform ::p/call '(call + 1 2))
          compiled (c/compile-call term)]
      (is (= Call (type compiled)))
      (is (= '+ (:function-symbol compiled)))
      (is (= 2 (count (:args compiled))))))
  (testing "compile-list"
    (let [term (s/conform ::p/list '[1 2 3])
          compiled (c/compile-list term)]
      (is (= TermList (type compiled)))
      (is (= 3 (count (:values compiled))))))
  (testing "compile-term variable"
    (let [term (s/conform ::p/term '?test)
          compiled (c/compile-term term)]
      (is (= Variable (type compiled)))
      (is (= '?test (:name compiled)))))
  (testing "compile-term constant"
    (let [term (s/conform ::p/term 'test)
          compiled (c/compile-term term)]
      (is (= Constant (type compiled)))
      (is (= 'test (:name compiled)))))
  (testing "compile-term number"
    (let [term (s/conform ::p/term 2)
          compiled (c/compile-term term)]
      (is (= TermNumber (type compiled)))
      (is (= 2 (:value compiled)))))
  (testing "compile-term call"
    (let [term (s/conform ::p/term '(call + 1 2))
          compiled (c/compile-term term)]
      (is (= Call (type compiled)))))
  (testing "compile-term list"
    (let [term (s/conform ::p/term '[1 2])
          compiled (c/compile-term term)]
      (is (= TermList (type compiled))))))

(deftest test-expression
  (testing "compile-conjunction"
    (let [expr (s/conform ::p/conjunction '(and (hello)))
          compiled (c/compile-conjunction expr)]
      (is (= Conjunction (type compiled)))
      (is (= 1 (count (:expressions compiled)))))
    (let [expr (s/conform ::p/conjunction '())
          compiled (c/compile-conjunction expr)]
      (is (= Conjunction (type compiled)))
      (is (= 0 (count (:expressions compiled))))))
  (testing "compile-disjunction"
    (let [expr (s/conform ::p/disjunction '(or (hello)))
          compiled (c/compile-disjunction expr)]
      (is (= Disjunction (type compiled)))
      (is (= 1 (count (:expressions compiled))))))
  (testing "compile-negation"
    (let [expr (s/conform ::p/negation '(not (hello)))
          compiled (c/compile-negation expr)]
      (is (= Negation (type compiled)))
      (is (not (nil? (:expression compiled))))))
  (testing "compile-implication"
    (let [expr (s/conform ::p/implication '(imply (sunny ?place) (nice-out ?place)))
          compiled (c/compile-implication expr)]
      (is (= Implication (type compiled)))
      (is (not (nil? (:lhs compiled))))
      (is (not (nil? (:rhs compiled))))))
  (testing "compile-universal-quantification"
    (let [expr (s/conform ::p/universal-quantification '(forall [?p] (package ?p) (in ?p ?t)))
          compiled (c/compile-universal-quantification expr)]
      (is (= UniversalQuantification (type compiled)))
      (is (= 1 (count (:variables compiled))))
      (is (= '[?p] (:variables compiled)))
      (is (not (nil? (:predicate compiled))))
      (is (not (nil? (:form compiled))))))
  (testing "compile-assignment"
    (let [expr (s/conform ::p/assignment '(def ?hello 1))
          compiled (c/compile-assignment expr)]
      (is (= Assignment (type compiled)))
      (is (= '?hello (:name compiled)))
      (is (= TermNumber (type (:value compiled))))))
  (testing "compile-call"
    (let [expr (s/conform ::p/call '(call >= ?hello 1))
          compiled (c/compile-call expr)]
      (is (= Call (type compiled)))
      (is (= '>= (:function-symbol compiled)))
      (is (= 2 (count (:args compiled))))))
  (testing "compile-logical-atom"
    (let [expr (s/conform ::p/logical-atom '(test ?hello))
          compiled (c/compile-logical-atom expr)]
      (is (= LogicalAtom (type compiled)))
      (is (= 'test (:name compiled)))
      (is (= 1 (count (:args compiled)))))
    (let [expr (s/conform ::p/logical-atom '?hello)
          compiled (c/compile-logical-atom expr)]
      (is (= LateBinding (type compiled)))
      (is (= '?hello (:name compiled)))))
  (testing "LogicalAtom"
    (let [a (c/compile-logical-atom (s/conform ::p/logical-atom '(test 1 (call > 1 2) hi ?hey [1 2 3])))
          b (c/compile-logical-atom (s/conform ::p/logical-atom '(test 1 (call > 1 2) hi ?hey [1 2 3])))]
      (is (= a b))))
  (testing "compile-expression"
    (let [expr (s/conform ::p/expression '(and (hello)))
          compiled (c/compile-expression expr)]
      (is (= Conjunction (type compiled))))
    (let [expr (s/conform ::p/expression '())
          compiled (c/compile-expression expr)]
      (is (= Conjunction (type compiled))))
    (let [expr (s/conform ::p/expression '(or (hello)))
          compiled (c/compile-expression expr)]
      (is (= Disjunction (type compiled))))
    (let [expr (s/conform ::p/expression '(not (hello)))
          compiled (c/compile-expression expr)]
      (is (= Negation (type compiled))))
    (let [expr (s/conform ::p/expression '(imply (sunny ?place) (nice-out ?place)))
          compiled (c/compile-expression expr)]
      (is (= Implication (type compiled))))
    (let [expr (s/conform ::p/expression '(forall [?p] (package ?p) (in ?p ?t)))
          compiled (c/compile-expression expr)]
      (is (= UniversalQuantification (type compiled))))
    (let [expr (s/conform ::p/expression '(def ?hello 1))
          compiled (c/compile-expression expr)]
      (is (= Assignment (type compiled))))
    (let [expr (s/conform ::p/expression '(call >= ?hello 1))
          compiled (c/compile-expression expr)]
      (is (= Call (type compiled))))
    (let [expr (s/conform ::p/expression '(test ?hello))
          compiled (c/compile-expression expr)]
      (is (= LogicalAtom (type compiled))))))

(deftest test-logical-precondition
  (testing "compile-first-satisfier-precondition"
    (let [precond (s/conform ::p/first-satisfier-precondition '(:first (test ?hello)))
          compiled (c/compile-first-satisfier-precondition precond)]
      (is (= FirstSatisfierPrecondition (type compiled)))
      (is (not (nil? (:expression compiled))))))
  (testing "compile-sorted-precondition"
    (let [precond (s/conform ::p/sorted-precondition '(:sort-by ?d > (and (at ?here)
                                                                          (distance ?her ?there ?d))))
          compiled (c/compile-sorted-precondition precond)]
      (is (= SortedPrecondition (type compiled)))
      (is (= '?d (:variable compiled)))
      (is (= '> (:comparator compiled)))
      (is (not (nil? (:expression compiled))))))
  (testing "compile-logical-precondition"
    (let [precond (s/conform ::p/logical-precondition '(:first (test ?hello)))
          compiled (c/compile-logical-precondition precond)]
      (is (= FirstSatisfierPrecondition (type compiled))))
    (let [precond (s/conform ::p/logical-precondition '(:sort-by ?d > (and (at ?here)
                                                                           (distance ?her ?there ?d))))
          compiled (c/compile-logical-precondition precond)]
      (is (= SortedPrecondition (type compiled))))
    (let [precond (s/conform ::p/logical-precondition '(and (at ?here)
                                                            (distance ?her ?there ?d)))
          compiled (c/compile-logical-precondition precond)]
      (is (= Conjunction (type compiled))))))

(deftest test-compile-axiom
  (testing "compile-axiom-precondition expression"
    (let [axiom (s/conform ::p/axiom '(:- (walking-distance ?x)
                                          good
                                          (and (weather-is good)
                                               (distance home ?x ?d)
                                               (call <= ?d 2))))
          expression (first (:axioms axiom))
          compiled (c/compile-axiom-precondition expression)]
      (is (= AxiomPrecondition (type compiled)))
      (is (= 'good (:name compiled)))
      (is (= Conjunction (type (:precondition compiled))))))
  (testing "compile-axiom-precondition first-satisfier"
    (let [axiom (s/conform ::p/axiom '(:- (walking-distance ?x)
                                          bad
                                          (:first
                                            (and (distance home ?x ?d)
                                                 (call <= ?d 1)))))
          first-satisfier (first (:axioms axiom))
          compiled (c/compile-axiom-precondition first-satisfier)]
      (is (= AxiomPrecondition (type compiled)))
      (is (= 'bad (:name compiled)))
      (is (= FirstSatisfierPrecondition (type (:precondition compiled))))))
  (testing "compile-axiom-precondition sorted"
    (let [axiom (s/conform ::p/axiom '(:- (walking-distance ?x)
                                          bad
                                          (:sort-by ?d > (and (at ?here)
                                                              (distance ?her ?there ?d)))))
          sorted (first (:axioms axiom))
          compiled (c/compile-axiom-precondition sorted)]
      (is (= AxiomPrecondition (type compiled)))
      (is (not (nil? (:name compiled))))
      (is (= SortedPrecondition (type (:precondition compiled))))))
  (let [axiom (s/conform ::p/axiom '(:- (walking-distance ?x)
                                        good
                                        (and (weather-is good)
                                             (distance home ?x ?d)
                                             (call <= ?d 2))
                                        bad
                                        (:first
                                         (and (distance home ?x ?d)
                                              (call <= ?d 1)))
                                        (:sort-by ?d < (and (at ?here)
                                                            (distance ?her ?there ?d)))))
        compiled (c/compile-axiom axiom)]
    (testing "axiom-name"
      (is (= 'walking-distance (c/axiom-name axiom))))
    (testing "compile-axiom"
      (is (= Axiom (type compiled)))
      (is (= 'walking-distance (:name compiled)))
      (is (= 1 (count (:args compiled))))
      (is (seqable? (:args compiled)))
      (is (= '[?x] (:args compiled)))
      (is (= 3 (count (:preconditions compiled))))
      (is (seqable? (:preconditions compiled))))))

(deftest test-operator
  (testing "compile-delete-add-element"
    (let [element (s/conform ::p/delete-add-element '(at ?truck ?old-loc))
          compiled (c/compile-delete-add-element element)]
      (is (= LogicalAtom (type compiled))))
    (let [element (s/conform ::p/delete-add-element '?test)
          compiled (c/compile-delete-add-element element)]
      (is (= LateBinding (type compiled))))
    (let [element (s/conform ::p/delete-add-element '(:protection (at ?truck ?old-loc)))
          compiled (c/compile-delete-add-element element)]
      (is (= ProtectionCondition (type compiled))))
    (let [element (s/conform ::p/delete-add-element '(forall [?p] (package ?p) (in ?p ?t)))
          compiled (c/compile-delete-add-element element)]
      (is (= UniversalQuantification (type compiled)))))
  (testing "compile-delete-add-list"
    (let [list (s/conform ::p/delete-add-list '[(at ?truck ?old-loc)
                                                (:protection (at ?truck ?old-loc))])
          compiled (c/compile-delete-add-list list)]
      (is (= 2 (count compiled))))
    (let [list (s/conform ::p/delete-add-list '?test)
          compiled (c/compile-delete-add-list list)]
      (is (= LateBinding (type compiled)))))
  (let [operator (s/conform ::p/operator '(:operator (!drive-to ?truck ?old-loc ?location)
                                                     ()
                                                     [(at ?truck ?old-loc)]
                                                     [(at ?truck ?location)
                                                      (:protection (at ?truck ?location))]))
        compiled (c/compile-operator operator)]
    (testing "operator-name"
      (is (= '!drive-to (c/operator-name operator))))
    (testing "compile-operator"
      (is (= Operator (type compiled)))
      (is (= '!drive-to (:name compiled)))
      (is (= 3 (count (:args compiled))))
      (is (= '[?truck ?old-loc ?location] (:args compiled)))
      (is (= Conjunction (type (:precondition compiled))))
      (is (= 1 (count (:delete-list compiled))))
      (is (= 2 (count (:add-list compiled)))))))

(deftest test-method
  (testing "compile-task-atom"
    (let [atom (s/conform ::p/task-atom '(!test ?hey))
          compiled (c/compile-task-atom atom)]
      (is (= TaskAtom (type compiled)))
      (is (= '!test (:name compiled)))
      (is (= 1 (count (:args compiled))))
      (is (false? (:immediate? compiled)))
      (is (true? (:primitive? compiled))))
    (let [atom (s/conform ::p/task-atom '(test ?hey))
          compiled (c/compile-task-atom atom)]
      (is (= TaskAtom (type compiled)))
      (is (= 'test (:name compiled)))
      (is (= 1 (count (:args compiled))))
      (is (false? (:immediate? compiled)))
      (is (false? (:primitive? compiled))))
    (let [atom (s/conform ::p/task-atom '(:immediate !test ?hey))
          compiled (c/compile-task-atom atom)]
      (is (= TaskAtom (type compiled)))
      (is (= '!test (:name compiled)))
      (is (= 1 (count (:args compiled))))
      (is (true? (:immediate? compiled)))
      (is (true? (:primitive? compiled))))
    (let [atom (s/conform ::p/task-atom '(:immediate test ?hey))
          compiled (c/compile-task-atom atom)]
      (is (= TaskAtom (type compiled)))
      (is (= 'test (:name compiled)))
      (is (= 1 (count (:args compiled))))
      (is (true? (:immediate? compiled)))
      (is (false? (:primitive? compiled)))))
  (testing "compile-task-list"
    (let [list (s/conform ::p/task-list '[[(!test ?hey)] (!eat-with-fork ?food ?fork)])
          compiled (c/compile-task-list list)]
      (is (= TaskList (type compiled)))
      (is (true? (:ordered? compiled)))
      (is (= 2 (count (:task-lists compiled)))))
    (let [list (s/conform ::p/task-list '[:unordered [(!test ?hey)] (!eat-with-fork ?food ?fork)])
          compiled (c/compile-task-list list)]
      (is (= TaskList (type compiled)))
      (is (false? (:ordered? compiled)))
      (is (= 2 (count (:task-lists compiled))))))
  (testing "compile-method-option"
    (let [method (s/conform ::p/method '(:method (eat ?food)
                                                 branch1
                                                 (have-fork ?fork)
                                                 [(!eat-with-fork ?food ?fork)]))
          method-option (first (:options method))
          compiled (c/compile-method-option method-option)]
      (is (= MethodOption (type compiled)))
      (is (= 'branch1 (:name compiled)))
      (is (= LogicalAtom (type (:precondition compiled))))
      (is (= TaskList (type (:tail compiled)))))
    (let [method (s/conform ::p/method '(:method (eat ?food)
                                                 (have-fork ?fork)
                                                 [(!eat-with-fork ?food ?fork)]))
          method-option (first (:options method))
          compiled (c/compile-method-option method-option)]
      (is (= MethodOption (type compiled)))
      (is (symbol? (:name compiled)))
      (is (= LogicalAtom (type (:precondition compiled))))
      (is (= TaskList (type (:tail compiled))))))
  (let [method (s/conform ::p/method '(:method (eat ?food)
                                               branch1
                                               (have-fork ?fork)
                                               [(!eat-with-fork ?food ?fork)]
                                               branch2
                                               (have-spoon ?spoon)
                                               [(!eat-with-spoon ?food ?spoon)]))
        compiled (c/compile-method method)]
    (testing "method-name"
      (is (= 'eat (c/method-name method))))
    (testing "compile-method"
      (is (= Method (type compiled)))
      (is (= 'eat (:name compiled)))
      (is (= 1 (count (:args compiled))))
      (is (= 2 (count (:options compiled)))))))

(deftest test-domain-extension
  (testing "compile-domain-extension"
    (let [domain (p/parse-domain-extension '[(:- (walking-distance ?x)
                                                 good
                                                 (and (weather-is good)
                                                      (distance home ?x ?d)
                                                      (call <= ?d 2))
                                                 bad
                                                 (and (distance home ?x ?d)
                                                      (call <= ?d 1)))
                                             (:method (eat ?food)
                                                      branch1
                                                      (have-fork ?fork)
                                                      [(!eat-with-fork ?food ?fork)]
                                                      branch2
                                                      (have-spoon ?spoon)
                                                      [(!eat-with-spoon ?food ?spoon)])
                                             (:operator (!drive-to ?truck ?old-loc ?location)
                                                        ()
                                                        [(at ?truck ?old-loc)]
                                                        [(at ?truck ?location)
                                                         (:protection (at ?truck ?location))])])
          compiled (c/compile-domain-extension domain)]
      (is (= DomainExtension (type compiled)))
      (is (= 1 (count (:axioms compiled))))
      (is (= Axiom (type (second (first (:axioms compiled))))))
      (is (= 1 (count (:methods compiled))))
      (is (= Method (type (second (first (:methods compiled))))))
      (is (= 1 (count (:operators compiled))))
      (is (= Operator (type (second (first (:operators compiled)))))))
    (let [domain (p/parse-domain-extension '[])
          compiled (c/compile-domain-extension domain)]
      (is (= DomainExtension (type compiled)))
      (is (empty? (:axioms compiled)))
      (is (empty? (:methods compiled)))
      (is (empty? (:operators compiled))))))

(deftest test-problem
  (testing "compile-problem"
    (let [problem (s/conform ::p/problem '(defproblem test
                                            [(hello 1)]
                                            [(!test) (eat) (!sleep)]))
          compiled (c/compile-problem problem)]
      (is (= 'test (:name compiled)))
      (is (= 1 (count (:initial-state compiled))))
      (is (= LogicalAtom (type (first (:initial-state compiled)))))
      (is (= TaskList (type (:task-list compiled))))
      (is (= 3 (count (:task-lists (:task-list compiled))))))))
