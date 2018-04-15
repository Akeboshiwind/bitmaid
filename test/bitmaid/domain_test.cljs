(ns bitmaid.domain-test
  (:require [bitmaid.domain :as domain]
            [cljs.test :as t :include-macros true :refer [deftest testing is]]
            [cljs.spec.alpha :as s]
            [expound.alpha :refer [expound]]))

(deftest test-symbols
  (testing "::variable-symbol"
    (is (s/valid? ::domain/variable-symbol '?hello))
    (is (not (s/valid? ::domain/variable-symbol '!hello)))
    (is (not (s/valid? ::domain/variable-symbol 'hello))))
  (testing "::task-symbol"
    (is (s/valid? ::domain/task-symbol '!hello))
    (is (not (s/valid? ::domain/task-symbol '?hello)))
    (is (not (s/valid? ::domain/task-symbol 'hello))))
  (testing "::constant-symbol"
    (is (s/valid? ::domain/constant-symbol 'hello))
    (is (s/valid? ::domain/constant-symbol '_hello))
    (is (not (s/valid? ::domain/constant-symbol '?hello)))
    (is (not (s/valid? ::domain/constant-symbol '!hello)))
    (is (not (s/valid? ::domain/constant-symbol 'and)))
    (is (not (s/valid? ::domain/constant-symbol 'not)))
    (is (not (s/valid? ::domain/constant-symbol 'call)))
    (is (not (s/valid? ::domain/constant-symbol 'or)))
    (is (not (s/valid? ::domain/constant-symbol 'imply)))
    (is (not (s/valid? ::domain/constant-symbol 'forall)))
    (is (not (s/valid? ::domain/constant-symbol 'def))))
  (testing "::predicate-symbol"
    (is (s/valid? ::domain/predicate-symbol 'hello))
    (is (s/valid? ::domain/predicate-symbol '_hello))
    (is (not (s/valid? ::domain/predicate-symbol '?hello)))
    (is (not (s/valid? ::domain/predicate-symbol '!hello))))
  (testing "::compound-task-symbol"
    (is (s/valid? ::domain/compound-task-symbol 'hello))
    (is (s/valid? ::domain/compound-task-symbol '_hello))
    (is (not (s/valid? ::domain/compound-task-symbol '?hello)))
    (is (not (s/valid? ::domain/compound-task-symbol '!hello))))
  (testing "::function-symbol"
    (is (s/valid? ::domain/function-symbol 'hello))
    (is (not (s/valid? ::domain/function-symbol '?hello)))
    (is (not (s/valid? ::domain/function-symbol '!hello)))))

(deftest test-basic-structures
  (testing "::list"
    (is (s/valid? ::domain/list '[vector of elements 123 123.213]))
    (is (s/valid? ::domain/list '[]))
    (is (not (s/valid? ::domain/list '(list of elements 123 123.213))))
    (is (not (s/valid? ::domain/list 'hello)))
    (is (not (s/valid? ::domain/list '?hello)))
    (is (not (s/valid? ::domain/list '!hello))))
  (testing "::call"
    (is (s/valid? ::domain/call '(call + 1 2 3)))
    (is (s/valid? ::domain/call '(call + ?hello 2 3)))
    (is (s/valid? ::domain/call '(call + ?hello 2.34 3)))
    (is (s/valid? ::domain/call '(call + (call - 1 2 3) 2.34 3)))
    (is (s/valid? ::domain/call '(call test 2.34 3 [1 2 3])))
    (is (not (s/valid? ::domain/call 'hello)))
    (is (not (s/valid? ::domain/call '?hello)))
    (is (not (s/valid? ::domain/call '!hello)))
    (is (not (s/valid? ::domain/call '(+ 2 3))))
    (is (not (s/valid? ::domain/call '(call + !hello 2 3)))))
  (testing "::term"
    (is (s/valid? ::domain/term 'hello))
    (is (= :constant-symbol
           (first (s/conform ::domain/term 'hello))))
    (is (s/valid? ::domain/term '?hello))
    (is (= :variable-symbol
           (first (s/conform ::domain/term '?hello))))
    (is (s/valid? ::domain/term 123))
    (is (= :number
           (first (s/conform ::domain/term 123))))
    (is (s/valid? ::domain/term 123.456))
    (is (= :number
           (first (s/conform ::domain/term 123.456))))
    (is (s/valid? ::domain/term '(call + 1 2 3)))
    (is (= :call
           (first (s/conform ::domain/term '(call + 1 2 3)))))
    (is (s/valid? ::domain/term '[1 2 3]))
    (is (= :list
           (first (s/conform ::domain/term '[1 2 3]))))
    (is (s/valid? ::domain/term '(call + [1 2 3] 2 3)))
    (is (s/valid? ::domain/term '(call + (call + 1 2 3 2 3))))
    (is (s/valid? ::domain/term '[[1 2 3] 2 3]))
    (is (s/valid? ::domain/term '[(call + 1 2 3) 2 3]))
    (is (not (s/valid? ::domain/term '(+ 1 2 3))))))

(deftest test-helper-functions
  (testing "restricted?"
    (is (domain/restricted? 'not))
    (is (domain/restricted? 'call))
    (is (domain/restricted? 'and))
    (is (domain/restricted? 'or))
    (is (domain/restricted? 'imply))
    (is (domain/restricted? 'forall))
    (is (domain/restricted? 'def))
    (is (not (domain/restricted? 'test)))
    (is (not (domain/restricted? 'hello)))
    (is (not (domain/restricted? '?hello)))
    (is (not (domain/restricted? '!hello)))
    (is (not (domain/restricted? '_hello))))
  (testing "walk-tree"
    (is (= [[1 2 3] 1 2 3]
           (domain/walk-tree [1 2 3])))
    (is (= [[]]
           (domain/walk-tree [])))
    (is (= [[1 [2 3]] 1 [2 3] 2 3]
           (domain/walk-tree [1 [2 3]]))))
  (testing "find-variables"
    (is (= #{1 2 3}
           (domain/find-variables [[:variable-symbol 1] {:test [[:variable-symbol 2]]
                                                         :another {:yet-again [[:variable-symbol 3]]}}]))))
  (testing "set="
    (is (domain/set= [1 2 3] [3 2 1]))
    (is (domain/set= #{1 2 3} [3 2 1]))
    (is (not (domain/set= [1 2] [3 2 1])))))

(deftest test-logical-structures
  (testing "::logical-atom"
    (is (s/valid? ::domain/logical-atom '(hello)))
    (is (s/valid? ::domain/logical-atom '(hello ?test)))
    (is (s/valid? ::domain/logical-atom '(hello test)))
    (is (s/valid? ::domain/logical-atom '(hello 1)))
    (is (s/valid? ::domain/logical-atom '(hello (call + 1 2))))
    (is (s/valid? ::domain/logical-atom '(hello [1 2 3])))
    (is (s/valid? ::domain/logical-atom '(hello 1 2 3)))
    (is (not (s/valid? ::domain/logical-atom '[hello 1 2 3])))
    (is (not (s/valid? ::domain/logical-atom '(?hello 1 2 3))))
    (is (not (s/valid? ::domain/logical-atom '(!hello 1 2 3))))
    (is (not (s/valid? ::domain/logical-atom '(not 1 2 3))))
    (is (not (s/valid? ::domain/logical-atom '(call 1 2 3))))
    (is (not (s/valid? ::domain/logical-atom '(or 1 2 3))))
    (is (not (s/valid? ::domain/logical-atom '(imply 1 2 3))))
    (is (not (s/valid? ::domain/logical-atom '(forall 1 2 3))))
    (is (not (s/valid? ::domain/logical-atom '(def 1 2 3))))
    (is (not (s/valid? ::domain/logical-atom '(and 1 2 3)))))
  (testing "::conjunction"
    (is (s/valid? ::domain/conjunction '(and (hello))))
    (is (s/valid? ::domain/conjunction '(and (and (hello ?test)) (and (hello)))))
    (is (s/valid? ::domain/conjunction '(and)))
    (is (s/valid? ::domain/conjunction '()))
    (is (not (s/valid? ::domain/conjunction '((hello)))))
    (is (not (s/valid? ::domain/conjunction '(and hello ?test)))))
  (testing "::disjunction"
    (is (s/valid? ::domain/disjunction '(or (hello))))
    (is (s/valid? ::domain/disjunction '(or (or (hello ?test)) (or (hello)))))
    (is (s/valid? ::domain/disjunction '(or)))
    (is (not (s/valid? ::domain/disjunction '(or hello ?test)))))
  (testing "::negation"
    (is (s/valid? ::domain/negation '(not (hello))))
    (is (s/valid? ::domain/negation '(not (hello ?test))))
    (is (not (s/valid? ::domain/negation '(not))))
    (is (not (s/valid? ::domain/disjunction '(not (hello ?test) (test)))))
    (is (not (s/valid? ::domain/disjunction '(not hello ?test)))))
  (testing "::implication"
    (is (s/valid? ::domain/implication '(imply (sunny ?place) (nice-out ?place))))
    (is (not (s/valid? ::domain/implication '(imply (sunny ?place) (nice-out ?place) (test ?hello)))))
    (is (not (s/valid? ::domain/implication '(imply sunny ?place))))
    (is (not (s/valid? ::domain/implication '(imply (sunny ?place)))))
    (is (not (s/valid? ::domain/implication '(imply)))))
  (testing "::universal-quantification"
    (is (s/valid? ::domain/universal-quantification
                  '(forall [?p] (package ?p) (in ?p ?t))))
    (is (not (s/valid? ::domain/universal-quantification
                       '(forall [?p] (package ?p)))))
    (is (not (s/valid? ::domain/universal-quantification
                       '(forall [?p]))))
    (is (not (s/valid? ::domain/universal-quantification
                       '(forall))))
    (is (not (s/valid? ::domain/universal-quantification
                       '(forall [?p] (package ?x) (in ?x ?t))))))
  (testing "::assignment"
    (is (s/valid? ::domain/assignment '(def ?test 1)))
    (is (s/valid? ::domain/assignment '(def ?test [1])))
    (is (s/valid? ::domain/assignment '(def ?test (call + 1 2))))
    (is (not (s/valid? ::domain/assignment '(def test 1))))
    (is (not (s/valid? ::domain/assignment '(def _test 1))))
    (is (not (s/valid? ::domain/assignment '(def !test 1)))))
  (testing "::expression"
    (is (s/valid? ::domain/expression '(test ?hello)))
    (is (= :logical-atom
           (first (s/conform ::domain/expression '(test ?hello)))))
    (is (s/valid? ::domain/expression '(and (hello))))
    (is (= :conjunction
           (first (s/conform ::domain/expression '(and (hello))))))
    (is (s/valid? ::domain/expression '()))
    (is (= :conjunction
           (first (s/conform ::domain/expression '()))))
    (is (s/valid? ::domain/expression '(or (hello))))
    (is (= :disjunction
           (first (s/conform ::domain/expression '(or (hello))))))
    (is (s/valid? ::domain/expression '(not (hello))))
    (is (= :negation
           (first (s/conform ::domain/expression '(not (hello))))))
    (is (s/valid? ::domain/expression '(imply (sunny ?place) (nice-out ?place))))
    (is (= :implication
           (first (s/conform ::domain/expression '(imply (sunny ?place) (nice-out ?place))))))
    (is (s/valid? ::domain/expression '(forall [?p] (package ?p) (in ?p ?t))))
    (is (= :universal-quantification
           (first (s/conform ::domain/expression '(forall [?p] (package ?p) (in ?p ?t))))))
    (is (s/valid? ::domain/expression '(def ?hello 1)))
    (is (= :assignment
           (first (s/conform ::domain/expression '(def ?hello 1)))))
    (is (s/valid? ::domain/expression '(call >= ?hello 1)))
    (is (= :call
           (first (s/conform ::domain/expression '(call >= ?hello 1)))))))

(deftest test-preconditions
  (testing "::first-satisfier-precondition"
    (is (s/valid? ::domain/first-satisfier-precondition '(:first (test ?hello))))
    (is (not (s/valid? ::domain/first-satisfier-precondition '(:first))))
    (is (not (s/valid? ::domain/first-satisfier-precondition '(:first (test ?hello)
                                                                      (another ?h)))))
    (is (not (s/valid? ::domain/first-satisfier-precondition '(:last (test ?hello))))))
  (testing "::sorted-precondition"
    (is (s/valid? ::domain/sorted-precondition '(:sort-by ?d > (and (at ?here)
                                                                    (distance ?her ?there ?d)))))
    (is (not (s/valid? ::domain/sorted-precondition '(:sort-by ?d >))))
    (is (not (s/valid? ::domain/sorted-precondition '(:sort-by ?d))))
    (is (not (s/valid? ::domain/sorted-precondition '(:sort-by))))
    (is (not (s/valid? ::domain/sorted-precondition '(:first ?d > (and (at ?here))
                                                             (distance ?her ?there ?d)))))
    (is (not (s/valid? ::domain/sorted-precondition '(:sort-by d > (and (at ?here)
                                                                        (distance ?her ?there ?d))))))
    (is (not (s/valid? ::domain/sorted-precondition '(:sort-by ?d ?x (and (at ?here)
                                                                          (distance ?her ?there ?d)))))))
  (testing "::logical-precondition"
    (is (s/valid? ::domain/logical-precondition '(test ?hello)))
    (is (= :expression
           (first (s/conform ::domain/logical-precondition '(test ?hello)))))
    (is (s/valid? ::domain/logical-precondition '(:first (test ?hello))))
    (is (= :first-satisfier-precondition
           (first (s/conform ::domain/logical-precondition '(:first (test ?hello))))))
    (is (s/valid? ::domain/logical-precondition '(:sort-by ?d > (and (at ?here)
                                                                     (distance ?her ?there ?d)))))
    (is (= :sorted-precondition
           (first (s/conform ::domain/logical-precondition '(:sort-by ?d > (and (at ?here)
                                                                                (distance ?her ?there ?d)))))))))

(deftest test-axiom
  (testing "::axiom"
    (is (s/valid? ::domain/axiom '(:- hello (name ?me))))
    (is (s/valid? ::domain/axiom '(:- hello (name ?me) test (name ?him))))
    (is (s/valid? ::domain/axiom '(:-)))
    (is (not (s/valid? ::domain/axiom '(:- hello))))
    (is (not (s/valid? ::domain/axiom '(:- ?hello (name ?me)))))
    (is (not (s/valid? ::domain/axiom '(:- !hello (name ?me)))))
    (is (not (s/valid? ::domain/axiom '(:- hello (name ?me) test))))
    (is (not (s/valid? ::domain/axiom '(:- hello test))))))

(deftest test-task-structures
  (testing "::task-atom"
    (is (s/valid? ::domain/task-atom '(!hello 1)))
    (is (s/valid? ::domain/task-atom '(:immediate !hello 1)))
    (is (s/valid? ::domain/task-atom '(hello 1)))
    (is (s/valid? ::domain/task-atom '(:immediate hello 1)))
    (is (s/valid? ::domain/task-atom '(!hello)))
    (is (s/valid? ::domain/task-atom '(:immediate !hello)))
    (is (s/valid? ::domain/task-atom '(hello)))
    (is (s/valid? ::domain/task-atom '(:immediate hello)))
    (is (not (s/valid? ::domain/task-atom '(:thing !hello 1)))))
  (testing "::task-list"
    (is (s/valid? ::domain/task-list '[(!hello 1)]))
    (is (s/valid? ::domain/task-list '[(hello 1)]))
    (is (s/valid? ::domain/task-list '[(!hello 1) (!hi 2)]))
    (is (s/valid? ::domain/task-list '[(hello 1) (!hi 2)]))
    (is (s/valid? ::domain/task-list '[(:immediate !hello 1)]))
    (is (s/valid? ::domain/task-list '[(:immediate hello 1)]))
    (is (s/valid? ::domain/task-list '[:unordered (!hello 1)]))
    (is (s/valid? ::domain/task-list '[:unordered (hello 1)]))
    (is (s/valid? ::domain/task-list '[:unordered (!hello 1) (!hi 2)]))
    (is (s/valid? ::domain/task-list '[:unordered (hello 1) (!hi 2)]))
    (is (s/valid? ::domain/task-list '[:unordered (:immediate !hello 1)]))
    (is (s/valid? ::domain/task-list '[:unordered (:immediate hello 1)]))
    (is (not (s/valid? ::domain/task-list '[:thing (!hello 1) (!hi 2)])))
    (is (not (s/valid? ::domain/task-list '[:thing (hello 1) (!hi 2)])))
    (is (not (s/valid? ::domain/task-list '[1 (!hi 2)])))
    (is (not (s/valid? ::domain/task-list '[1 (hi 2)]))))
  (testing "::task-list"
    (is (s/valid? ::domain/task-list '[(!hello 1)]))))

(deftest test-operator
  (testing "::protection-condition"
    (is (s/valid? ::domain/protection-condition '(:protection (at ?truck ?location))))
    (is (s/valid? ::domain/protection-condition '(:protection (hello (call + 1 2)))))
    (is (not (s/valid? ::domain/protection-condition '(:this (at ?truck ?location))))))
  (testing "::delete-add-element"
    (is (s/valid? ::domain/delete-add-element '(at ?truck ?location)))
    (is (s/valid? ::domain/delete-add-element '(:protection (at ?truck ?location))))
    (is (s/valid? ::domain/delete-add-element '(:protection (hello (call + 1 2)))))
    (is (s/valid? ::domain/delete-add-element '(forall [?package] (blue ?package) (not (red ?package)))))
    (is (not (s/valid? ::domain/delete-add-element '(:this (at ?truck ?location)))))
    (is (not (s/valid? ::domain/delete-add-element '(imply (at ?truck ?location))))))
  (testing "::operator"
    (is (s/valid? ::domain/operator '(:operator (!drive-to ?truck ?old-loc ?location)
                                                ()
                                                [(at ?truck ?old-loc)]
                                                [(at ?truck ?location)
                                                 (:protection (at ?truck ?location))])))
    (is (s/valid? ::domain/operator '(:operator (!pick-up ?truck ?package ?location)
                                                ()
                                                [(at ?package ?location)
                                                 (:protection (at ?truck ?location))
                                                 (forall [?package] (blue ?package) (not (red ?package)))]
                                                [(in ?package ?truck)])))
    (is (s/valid? ::domain/operator '(:operator (!drive-to ?truck ?old-loc ?location)
                                                ()
                                                [(at ?truck ?old-loc)]
                                                [(at ?truck ?location)
                                                 (:protection (at ?truck ?location))]
                                                2)))
    (is (s/valid? ::domain/operator '(:operator (!drive-to ?truck ?old-loc ?location)
                                                ()
                                                []
                                                [])))
    (is (= :normal-task
           (first
            (:head
             (s/conform ::domain/operator '(:operator (!drive-to ?truck ?old-loc ?location)
                                                      ()
                                                      [(at ?truck ?old-loc)]
                                                      [(at ?truck ?location)
                                                       (:protection (at ?truck ?location))]))))))
    (is (s/valid? ::domain/operator '(:operator (!drive-to ?truck ?old-loc ?location)
                                                (:first (test ?hello))
                                                [(at ?truck ?old-loc)]
                                                [(at ?truck ?location)
                                                 (:protection (at ?truck ?location))])))
    (is (s/valid? ::domain/operator '(:operator (!drive-to ?truck ?old-loc ?location)
                                                (test ?hello)
                                                [(at ?truck ?old-loc)]
                                                [(at ?truck ?location)
                                                 (:protection (at ?truck ?location))])))
    (is (s/valid? ::domain/operator '(:operator (!drive-to ?truck ?old-loc ?location)
                                                (test ?hello)
                                                [(at ?hello ?old-loc)]
                                                [(at ?truck ?location)
                                                 (:protection (at ?truck ?location))])))
    (is (not (s/valid? ::domain/operator '(:operator (:immediate !drive-to ?truck ?old-loc ?location)
                                                    ()
                                                    [(at ?truck ?old-loc)]
                                                    [(at ?truck ?location)]
                                                    (:protection (at ?truck ?location))))))
    (is (not (s/valid? ::domain/operator '(:operator (?drive-to ?truck ?old-loc ?location)
                                                     ()
                                                     [(at ?truck ?old-loc)]
                                                     [(at ?truck ?location)]
                                                     (:protection (at ?truck ?location))))))
    (is (not (s/valid? ::domain/operator '(:operator (drive-to ?truck ?old-loc ?location)
                                                     ()
                                                     [(at ?truck ?old-loc)]
                                                     [(at ?truck ?location)]
                                                     (:protection (at ?truck ?location))))))
    (is (not (s/valid? ::domain/operator '(:operator (!drive-to ?truck ?old-loc ?location)
                                                     ()
                                                     [(at ?truck ?old-loc)]
                                                     [(at ?truck ?location)]
                                                     (:protection (at ?truck ?location))
                                                     2 3))))
    (is (not (s/valid? ::domain/operator '(:operator (!drive-to ?truck ?old-loc ?location)
                                                     ()
                                                     [(at ?a ?old-loc)]
                                                     [(at ?truck ?location)
                                                      (:protection (at ?truck ?location))]))))))

(deftest test-method
  (testing "::method"
    (is (s/valid? ::domain/method
                  '(:method (eat ?food)
                            branch1
                            (have-fork ?fork)
                            [(!eat-with-fork ?food ?fork)]
                            branch2
                            (have-spoon ?spoon)
                            [(!eat-with-spoon ?food ?spoon)])))
    (is (= 2
           (count
            (:options
             (s/conform ::domain/method
                        '(:method (eat ?food)
                                  branch1
                                  (have-fork ?fork)
                                  [(!eat-with-fork ?food ?fork)]
                                  branch2
                                  (have-spoon ?spoon)
                                  [(!eat-with-spoon ?food ?spoon)]))))))
    (is (s/valid? ::domain/method
                  '(:method (eat ?food)
                            branch1
                            (have-fork ?fork)
                            [(!eat-with-fork ?food ?fork)])))
    (is (= 1
           (count
            (:options
             (s/conform ::domain/method
                        '(:method (eat ?food)
                                  branch1
                                  (have-fork ?fork)
                                  [(!eat-with-fork ?food ?fork)]))))))
    (is (s/valid? ::domain/method
                  '(:method (eat ?food)
                            branch2
                            (and (not (have-fork ?fork))
                                 (have-spoon ?spoon))
                            [(!eat-with-spoon ?food ?spoon)])))
    (is (s/valid? ::domain/method
                  '(:method (eat ?food)
                            (and (not (have-fork ?fork))
                                 (have-spoon ?spoon))
                            [(!eat-with-spoon ?food ?spoon)])))
    (is (s/valid? ::domain/method
                  '(:method (eat ?food)
                            (have-fork ?fork)
                            [(!eat-with-fork ?food ?fork)]
                            (have-spoon ?spoon)
                            [(!eat-with-spoon ?food ?spoon)])))
    (is (s/valid? ::domain/method
                  '(:method (eat ?food)
                            branch1
                            ()
                            [(!eat-with-spoon ?food ?spoon)])))
    (is (s/valid? ::domain/method
                  '(:method (eat)
                            ()
                            [(eat-with-spoon)])))
    (is (= 2
           (count
            (:options
             (s/conform ::domain/method
                        '(:method (eat ?food)
                                  (have-fork ?fork)
                                  [(!eat-with-fork ?food ?fork)]
                                  (have-spoon ?spoon)
                                  [(!eat-with-spoon ?food ?spoon)]))))))
    (is (s/valid? ::domain/method
                  '(:method (eat ?food)
                            branch1
                            (have-fork ?fork)
                            [(!eat-with-fork ?food ?fork)]
                            (have-spoon ?spoon)
                            [(!eat-with-spoon ?food ?spoon)])))
    (is (= 2
           (count
            (:options
             (s/conform ::domain/method
                        '(:method (eat ?food)
                                  branch1
                                  (have-fork ?fork)
                                  [(!eat-with-fork ?food ?fork)]
                                  (have-spoon ?spoon)
                                  [(!eat-with-spoon ?food ?spoon)]))))))
    (is (not (s/valid? ::domain/method
                       '(:method (!eat ?food)
                                 branch1
                                 (have-fork ?fork)
                                 [(!eat-with-fork ?food ?fork)]
                                 branch2
                                 (have-spoon ?spoon)
                                 [(!eat-with-spoon ?food ?spoon)]))))
    (is (not (s/valid? ::domain/method
                       '(:method (?eat ?food)
                                 branch1
                                 (have-fork ?fork)
                                 [(!eat-with-fork ?food ?fork)]
                                 branch2
                                 (have-spoon ?spoon)
                                 [(!eat-with-spoon ?food ?spoon)]))))
    (is (not (s/valid? ::domain/method
                       '(:method (eat ?food)
                                 not
                                 (have-fork ?fork)
                                 [(!eat-with-fork ?food ?fork)]
                                 branch2
                                 (have-spoon ?spoon)
                                 [(!eat-with-spoon ?food ?spoon)]))))
    (is (not (s/valid? ::domain/method
                       '(:method (eat ?food)
                                 branch1
                                 (!have-fork ?fork)
                                 [(!eat-with-fork ?food ?fork)]
                                 branch2
                                 (have-spoon ?spoon)
                                 [(!eat-with-spoon ?food ?spoon)]))))))

(deftest test-domain-extension
  (testing "::domain-extension"
    (is (s/valid? ::domain/domain-extension '[(:method (have-breakfast)
                                                       ()
                                                       [(choose-drink) (make-meal) (!eat)])]))
    (is (s/valid? ::domain/domain-extension '[(:method (have-breakfast)
                                                       test-branch
                                                       ()
                                                       [(choose-drink) (make-meal) (!eat)])]))
    (is (s/valid? ::domain/domain-extension '[(:method (have-breakfast)
                                                       :test-branch
                                                       ()
                                                       [(choose-drink) (make-meal) (!eat)])]))
    (is (s/valid? ::domain/domain-extension '[(:method (have-breakfast)
                                                       ()
                                                       [(choose-drink) (make-meal) (!eat)])
                                              (:method (choose-drink)
                                                       :branch-coffe
                                                       (has-coffee ?coffee)
                                                       [(!make-drink ?coffee)]
                                                       :branch-tea
                                                       (has-tea ?tea)
                                                       [(!make-drink ?tea)])
                                              (:method (make-meal)
                                                       :ham-sandwich
                                                       (and (has-bread ?bread)
                                                           (has-butter ?butter)
                                                           (has-ham ?ham))
                                                       [(!make-sandwich ?bread ?butter ?ham)]
                                                       :eggs
                                                       (has-eggs ?eggs)
                                                       [(!make-eggs ?eggs)])]))))

(t/run-tests 'bitmaid.domain-test)
