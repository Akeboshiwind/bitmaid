(ns bitmaid.domain.executor-test
  (:require [bitmaid.domain.executor :as e]
            [bitmaid.domain.parser :as p]
            [bitmaid.domain.compiler :as c]
            [clojure.spec.alpha :as s]
            [clojure.test :as t :include-macros true :refer [deftest testing is]]))

(deftest JSOP2-compiler
  (testing "Variable"
    (is (= '?test
           (e/compile (c/map->Variable {:name '?test})))))
  (testing "Constant"
    (is (= 'test
           (e/compile (c/map->Constant {:name 'test})))))
  (testing "TermNumber"
    (is (= 123
           (e/compile (c/map->TermNumber {:value 123})))))
  (testing "Call"
    (is (= '(call + ?one two [1 2 3] 4 (call > 1 2))
           (e/compile
            (c/compile-call
             (s/conform ::p/call '(call + ?one two [1 2 3] 4 (call > 1 2))))))))
  (testing "TermList"
    (is (= '[?one two [1 2 3] 4 (call > 1 2)]
           (e/compile
            (c/compile-list
             (s/conform ::p/list '[?one two [1 2 3] 4 (call > 1 2)]))))))
  (testing "Conjunction"
    (is (= '(and (hello ?test) (hi ?another))
           (e/compile
            (c/compile-conjunction
             (s/conform ::p/conjunction '(and (hello ?test) (hi ?another)))))))
    (is (= '()
           (e/compile
            (c/compile-conjunction
             (s/conform ::p/conjunction '()))))))
  (testing "Disjunction"
    (is (= '(or (hello ?test) (hi ?another))
           (e/compile
            (c/compile-disjunction
             (s/conform ::p/disjunction '(or (hello ?test) (hi ?another)))))))
    (is (= '(or)
           (e/compile
            (c/compile-disjunction
             (s/conform ::p/disjunction '(or)))))))
  (testing "Negation"
    (is (= '(not (hello ?test))
           (e/compile
            (c/compile-negation
             (s/conform ::p/negation '(not (hello ?test))))))))
  (testing "Implication"
    (is (= '(imply (sunny ?place) (nice-out ?place))
           (e/compile
            (c/compile-implication
             (s/conform ::p/implication '(imply (sunny ?place) (nice-out ?place))))))))
  (testing "UniversalQuantification"
    (is (= '(forall (?p) (package ?p) (in ?p ?t))
           (e/compile
            (c/compile-universal-quantification
             (s/conform ::p/universal-quantification '(forall [?p] (package ?p) (in ?p ?t))))))))
  (testing "Assignment"
    (is (= '(assign ?test 1)
           (e/compile
            (c/compile-assignment
             (s/conform ::p/assignment '(def ?test 1)))))))
  (testing "LateBinding"
    (is (= '?test
           (e/compile
            (c/compile-logical-atom
             (s/conform ::p/logical-atom '?test))))))
  (testing "LateBinding"
    (is (= '(hello ?test 1)
           (e/compile
            (c/compile-logical-atom
             (s/conform ::p/logical-atom '(hello ?test 1)))))))
  (testing "FirstSatisfierPrecondition"
    (is (= '(:first (test ?hello))
           (e/compile
            (c/compile-first-satisfier-precondition
             (s/conform ::p/first-satisfier-precondition '(:first (test ?hello))))))))
  (testing "SortedPrecondition"
    (is (= '(:sort-by ?d > (and (at ?here)
                                (distance ?her ?there ?d)))
           (e/compile
            (c/compile-sorted-precondition
             (s/conform ::p/sorted-precondition '(:sort-by ?d > (and (at ?here)
                                                                     (distance ?her ?there ?d)))))))))
  (testing "TaskAtom"
    (is (= '(!test ?hey hi 1 [1 2 3])
           (e/compile
            (c/compile-task-atom
             (s/conform ::p/task-atom '(!test ?hey hi 1 [1 2 3]))))))
    (is (= '(:immediate !test ?hey hi 1 [1 2 3])
           (e/compile
            (c/compile-task-atom
             (s/conform ::p/task-atom '(:immediate !test ?hey hi 1 [1 2 3]))))))
    (is (= '(test ?hey hi 1 [1 2 3])
           (e/compile
            (c/compile-task-atom
             (s/conform ::p/task-atom '(test ?hey hi 1 [1 2 3]))))))
    (is (= '(:immediate test ?hey hi 1 [1 2 3])
           (e/compile
            (c/compile-task-atom
             (s/conform ::p/task-atom '(:immediate test ?hey hi 1 [1 2 3])))))))
  (testing "TaskList"
    (is (= '(((!test ?hey)) (!eat-with-fork ?food ?fork))
           (e/compile
            (c/compile-task-list
             (s/conform ::p/task-list '[[(!test ?hey)] (!eat-with-fork ?food ?fork)])))))
    (is (not (= clojure.lang.PersistentVector
                (type
                 (e/compile
                  (c/compile-task-list
                   (s/conform ::p/task-list '[[(!test ?hey)] (!eat-with-fork ?food ?fork)])))))))
    (is (= '(:unordered ((!test ?hey)) (!eat-with-fork ?food ?fork))
           (e/compile
            (c/compile-task-list
             (s/conform ::p/task-list '[:unordered [(!test ?hey)] (!eat-with-fork ?food ?fork)])))))
    (is (not (= clojure.lang.PersistentVector
                (type
                 (e/compile
                  (c/compile-task-list
                   (s/conform ::p/task-list '[:unordered [(!test ?hey)] (!eat-with-fork ?food ?fork)]))))))))
  (testing "MethodOption"
    (let [method (s/conform ::p/method '(:method (eat ?food)
                                                 branch1
                                                 (have-fork ?fork)
                                                 [(!eat-with-fork ?food ?fork)]))
          method-option (first (:options method))
          compiled (c/compile-method-option method-option)]
      (is (= '(branch1 (have-fork ?fork) ((!eat-with-fork ?food ?fork)))
             (e/compile compiled)))))
  (testing "Method"
    (let [method (s/conform ::p/method '(:method (eat ?food)
                                                 branch1
                                                 (have-fork ?fork)
                                                 [(!eat-with-fork ?food ?fork)]))
          compiled (c/compile-method method)]
      (is (= '(:method (eat ?food)
                       branch1
                       (have-fork ?fork)
                       ((!eat-with-fork ?food ?fork)))
             (e/compile compiled))))
    (let [method (s/conform ::p/method '(:method (eat ?food)
                                                 branch1
                                                 (have-fork ?fork)
                                                 [(!eat-with-fork ?food ?fork)]
                                                 branch2
                                                 (have-spoon ?spoon)
                                                 [(!eat-with-spoon ?food ?spoon)]))
          compiled (c/compile-method method)]
      (is (= '(:method (eat ?food)
                       branch1
                       (have-fork ?fork)
                       [(!eat-with-fork ?food ?fork)]
                       branch2
                       (have-spoon ?spoon)
                       [(!eat-with-spoon ?food ?spoon)])
             (e/compile compiled))))
    (let [method (s/conform ::p/method '(:method (eat ?food)
                                                 (have-fork ?fork)
                                                 [(!eat-with-fork ?food ?fork)]))
          compiled (c/compile-method method)]
      (is (e/compile compiled)))
    (let [method (s/conform ::p/method '(:method (eat ?food)
                                                 (have-fork ?fork)
                                                 [(!eat-with-fork ?food ?fork)]
                                                 (have-spoon ?spoon)
                                                 [(!eat-with-spoon ?food ?spoon)]))
          compiled (c/compile-method method)]
      (is (e/compile compiled))))
  (testing "ProtectionCondition"
    (is (= '(:protection (at ?truck ?old-loc))
           (e/compile
            (c/compile-delete-add-element
             (s/conform ::p/delete-add-element '(:protection (at ?truck ?old-loc))))))))
  (testing "Operator"
    (let [operator (s/conform ::p/operator '(:operator (!drive-to ?truck ?old-loc ?location)
                                                       ()
                                                       [(at ?truck ?old-loc)]
                                                       [(at ?truck ?location)
                                                        (:protection (at ?truck ?location))]))
          compiled (c/compile-operator operator)]
      (is (= '(:operator (!drive-to ?truck ?old-loc ?location)
                         ()
                         ((at ?truck ?old-loc))
                         ((at ?truck ?location)
                          (:protection (at ?truck ?location)))
                         1)
             (e/compile compiled)))))
  (testing "AxiomPrecondition"
    (let [axiom (s/conform ::p/axiom '(:- (eat ?food)
                                          branch1
                                          (have-fork ?fork)))
          precondition (first (:axioms axiom))
          compiled (c/compile-axiom-precondition precondition)]
      (is (= '(branch1 (have-fork ?fork))
             (e/compile compiled)))))
  (testing "Axiom"
    (let [axiom (s/conform ::p/axiom '(:- (walking-distance ?x)
                                          good
                                          (and (weather-is good)
                                               (distance home ?x ?d)
                                               (call <= ?d 2))
                                          bad
                                          (:first
                                           (and (distance home ?x ?d)
                                                (call <= ?d 1)))))
          compiled (c/compile-axiom axiom)]
      (is (= '(:- (walking-distance ?x)
                  good
                  (and (weather-is good)
                       (distance home ?x ?d)
                       (call <= ?d 2))
                  bad
                  (:first
                   (and (distance home ?x ?d)
                        (call <= ?d 1))))
             (e/compile compiled))))
    (let [axiom (s/conform ::p/axiom '(:- (walking-distance ?x)
                                          (and (weather-is good)
                                               (distance home ?x ?d)
                                               (call <= ?d 2))
                                          (:first
                                           (and (distance home ?x ?d)
                                                (call <= ?d 1)))))
          compiled (c/compile-axiom axiom)]
      (is (e/compile compiled))))
  (testing "DomainExtension"
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
      (is (= '(defdomain housedomain
                ((:- (walking-distance ?x)
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
                          ((!eat-with-fork ?food ?fork))
                          branch2
                          (have-spoon ?spoon)
                          ((!eat-with-spoon ?food ?spoon)))
                 (:operator (!drive-to ?truck ?old-loc ?location)
                            ()
                            ((at ?truck ?old-loc))
                            ((at ?truck ?location)
                             (:protection (at ?truck ?location)))
                            1)))
             (e/compile compiled)))))
  (testing "Problem"
    (let [problem (p/parse-problem '(defproblem problem
                                      [(have kiwi)]
                                      [(swap banjo kiwi)]))
          compiled (c/compile-problem problem)]
      (is (= '(defproblem problem housedomain
                ((have kiwi))
                ((swap banjo kiwi)))
             (e/compile compiled)))))
  [])
