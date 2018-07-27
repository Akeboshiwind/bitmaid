(ns bitmaid.domain.parser-test
  (:require [bitmaid.domain.parser :as p]
            [clojure.test :as t :include-macros true :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [expound.alpha :refer [expound]]))

(deftest test-symbols
  (testing "::variable-symbol"
    (is (s/valid? ::p/variable-symbol '?hello))
    (is (not (s/valid? ::p/variable-symbol '!hello)))
    (is (not (s/valid? ::p/variable-symbol 'hello))))
  (testing "::task-symbol"
    (is (s/valid? ::p/task-symbol '!hello))
    (is (not (s/valid? ::p/task-symbol '?hello)))
    (is (not (s/valid? ::p/task-symbol 'hello))))
  (testing "::constant-symbol"
    (is (s/valid? ::p/constant-symbol 'hello))
    (is (s/valid? ::p/constant-symbol '_hello))
    (is (not (s/valid? ::p/constant-symbol '?hello)))
    (is (not (s/valid? ::p/constant-symbol '!hello)))
    (is (not (s/valid? ::p/constant-symbol 'and)))
    (is (not (s/valid? ::p/constant-symbol 'not)))
    (is (not (s/valid? ::p/constant-symbol 'call)))
    (is (not (s/valid? ::p/constant-symbol 'or)))
    (is (not (s/valid? ::p/constant-symbol 'imply)))
    (is (not (s/valid? ::p/constant-symbol 'forall)))
    (is (not (s/valid? ::p/constant-symbol 'def))))
  (testing "::predicate-symbol"
    (is (s/valid? ::p/predicate-symbol 'hello))
    (is (s/valid? ::p/predicate-symbol '_hello))
    (is (not (s/valid? ::p/predicate-symbol '?hello)))
    (is (not (s/valid? ::p/predicate-symbol '!hello))))
  (testing "::compound-task-symbol"
    (is (s/valid? ::p/compound-task-symbol 'hello))
    (is (s/valid? ::p/compound-task-symbol '_hello))
    (is (not (s/valid? ::p/compound-task-symbol '?hello)))
    (is (not (s/valid? ::p/compound-task-symbol '!hello))))
  (testing "::function-symbol"
    (is (s/valid? ::p/function-symbol 'hello))
    (is (not (s/valid? ::p/function-symbol '?hello)))
    (is (not (s/valid? ::p/function-symbol '!hello)))))

(deftest test-basic-structures
  (testing "::list"
    (is (s/valid? ::p/list '[vector of elements 123 123.213]))
    (is (s/valid? ::p/list '[]))
    (is (not (s/valid? ::p/list '(list of elements 123 123.213))))
    (is (not (s/valid? ::p/list 'hello)))
    (is (not (s/valid? ::p/list '?hello)))
    (is (not (s/valid? ::p/list '!hello))))
  (testing "::call"
    (is (s/valid? ::p/call '(call + 1 2 3)))
    (is (s/valid? ::p/call '(call + ?hello 2 3)))
    (is (s/valid? ::p/call '(call + ?hello 2.34 3)))
    (is (s/valid? ::p/call '(call + (call - 1 2 3) 2.34 3)))
    (is (s/valid? ::p/call '(call test 2.34 3 [1 2 3])))
    (is (not (s/valid? ::p/call 'hello)))
    (is (not (s/valid? ::p/call '?hello)))
    (is (not (s/valid? ::p/call '!hello)))
    (is (not (s/valid? ::p/call '(+ 2 3))))
    (is (not (s/valid? ::p/call '(call + !hello 2 3)))))
  (testing "::term"
    (is (s/valid? ::p/term 'hello))
    (is (= :constant-symbol
           (first (s/conform ::p/term 'hello))))
    (is (s/valid? ::p/term '?hello))
    (is (= :variable-symbol
           (first (s/conform ::p/term '?hello))))
    (is (s/valid? ::p/term 123))
    (is (= :number
           (first (s/conform ::p/term 123))))
    (is (s/valid? ::p/term 123.456))
    (is (= :number
           (first (s/conform ::p/term 123.456))))
    (is (s/valid? ::p/term '(call + 1 2 3)))
    (is (= :call
           (first (s/conform ::p/term '(call + 1 2 3)))))
    (is (s/valid? ::p/term '[1 2 3]))
    (is (= :list
           (first (s/conform ::p/term '[1 2 3]))))
    (is (s/valid? ::p/term '(call + [1 2 3] 2 3)))
    (is (s/valid? ::p/term '(call + (call + 1 2 3 2 3))))
    (is (s/valid? ::p/term '[[1 2 3] 2 3]))
    (is (s/valid? ::p/term '[(call + 1 2 3) 2 3]))
    (is (not (s/valid? ::p/term '(+ 1 2 3))))))

(deftest test-helper-functions
  (testing "restricted?"
    (is (p/restricted? 'not))
    (is (p/restricted? 'call))
    (is (p/restricted? 'and))
    (is (p/restricted? 'or))
    (is (p/restricted? 'imply))
    (is (p/restricted? 'forall))
    (is (p/restricted? 'def))
    (is (not (p/restricted? 'test)))
    (is (not (p/restricted? 'hello)))
    (is (not (p/restricted? '?hello)))
    (is (not (p/restricted? '!hello)))
    (is (not (p/restricted? '_hello))))
  (testing "walk-tree"
    (is (= [[1 2 3] 1 2 3]
           (p/walk-tree [1 2 3])))
    (is (= [[]]
           (p/walk-tree [])))
    (is (= [[1 [2 3]] 1 [2 3] 2 3]
           (p/walk-tree [1 [2 3]]))))
  (testing "find-variables"
    (is (= #{1 2 3}
           (p/find-variables [[:variable-symbol 1] {:test [[:variable-symbol 2]]
                                                    :another {:yet-again [[:variable-symbol 3]]}}]))))
  (testing "set="
    (is (p/set= [1 2 3] [3 2 1]))
    (is (p/set= #{1 2 3} [3 2 1]))
    (is (not (p/set= [1 2] [3 2 1]))))
  (testing "ground?"
    (is (p/ground? '{:defproblem defproblem,
                     :initial-state
                     [{:predicate-symbol hello, :terms [[:number 1]]}
                      {:predicate-symbol hello, :terms [[:number 1]]}],
                     :task-list [:ordered {}]}))
    (is (not (p/ground? '{:defproblem defproblem,
                          :initial-state
                          [{:predicate-symbol hello, :terms [[:number 1]]}
                           {:predicate-symbol hello, :terms [[:variable-symbol ?test]]}],
                          :task-list [:ordered {}]})))))

(deftest test-logical-structures
  (testing "::logical-atom"
    (is (s/valid? ::p/logical-atom '(hello)))
    (is (= :logical-atom (first (s/conform ::p/logical-atom '(hello)))))
    (is (s/valid? ::p/logical-atom '(hello ?test)))
    (is (s/valid? ::p/logical-atom '(hello test)))
    (is (s/valid? ::p/logical-atom '(hello 1)))
    (is (s/valid? ::p/logical-atom '(hello (call + 1 2))))
    (is (s/valid? ::p/logical-atom '(hello [1 2 3])))
    (is (s/valid? ::p/logical-atom '(hello 1 2 3)))
    (is (s/valid? ::p/logical-atom '?test))
    (is (= :late-binding (first (s/conform ::p/logical-atom '?test))))
    (is (not (s/valid? ::p/logical-atom '[hello 1 2 3])))
    (is (not (s/valid? ::p/logical-atom '(?hello 1 2 3))))
    (is (not (s/valid? ::p/logical-atom '(!hello 1 2 3))))
    (is (not (s/valid? ::p/logical-atom '(not 1 2 3))))
    (is (not (s/valid? ::p/logical-atom '(call 1 2 3))))
    (is (not (s/valid? ::p/logical-atom '(or 1 2 3))))
    (is (not (s/valid? ::p/logical-atom '(imply 1 2 3))))
    (is (not (s/valid? ::p/logical-atom '(forall 1 2 3))))
    (is (not (s/valid? ::p/logical-atom '(def 1 2 3))))
    (is (not (s/valid? ::p/logical-atom '(and 1 2 3)))))
  (testing "::conjunction"
    (is (s/valid? ::p/conjunction '(and (hello))))
    (is (s/valid? ::p/conjunction '(and (and (hello ?test)) (and (hello)))))
    (is (s/valid? ::p/conjunction '(and)))
    (is (s/valid? ::p/conjunction '()))
    (is (s/valid? ::p/conjunction '(and (hello) ?test)))
    (is (s/valid? ::p/conjunction '(and (call + ?hello))))
    (is (not (s/valid? ::p/conjunction '((hello)))))
    (is (not (s/valid? ::p/conjunction '(and hello ?test)))))
  (testing "::disjunction"
    (is (s/valid? ::p/disjunction '(or (hello))))
    (is (s/valid? ::p/disjunction '(or (or (hello ?test)) (or (hello)))))
    (is (s/valid? ::p/disjunction '(or)))
    (is (s/valid? ::p/disjunction '(or ?hello)))
    (is (not (s/valid? ::p/disjunction '(or hello ?test)))))
  (testing "::negation"
    (is (s/valid? ::p/negation '(not (hello))))
    (is (s/valid? ::p/negation '(not (hello ?test))))
    (is (s/valid? ::p/negation '(not ?test)))
    (is (not (s/valid? ::p/negation '(not))))
    (is (not (s/valid? ::p/disjunction '(not (hello ?test) (test)))))
    (is (not (s/valid? ::p/disjunction '(not hello ?test)))))
  (testing "::implication"
    (is (s/valid? ::p/implication '(imply (sunny ?place) (nice-out ?place))))
    (is (s/valid? ::p/implication '(imply ?test (nice-out ?place))))
    (is (s/valid? ::p/implication '(imply (sunny ?place) ?test)))
    (is (s/valid? ::p/implication '(imply ?test ?another)))
    (is (not (s/valid? ::p/implication '(imply (sunny ?place) (nice-out ?place) (test ?hello)))))
    (is (not (s/valid? ::p/implication '(imply sunny ?place))))
    (is (not (s/valid? ::p/implication '(imply (sunny ?place)))))
    (is (not (s/valid? ::p/implication '(imply)))))
  (testing "::universal-quantification"
    (is (s/valid? ::p/universal-quantification '(forall [?p] (package ?p) (in ?p ?t))))
    (is (s/valid? ::p/universal-quantification '(forall [?p] ?p ?test)))
    (is (s/valid? ::p/universal-quantification '(forall [?p] (package ?p) ?test)))
    (is (s/valid? ::p/universal-quantification '(forall [?p] ?test (in ?p ?t))))
    (is (not (s/valid? ::p/universal-quantification '(forall [?p] (package ?p)))))
    (is (not (s/valid? ::p/universal-quantification '(forall [?p]))))
    (is (not (s/valid? ::p/universal-quantification '(forall))))
    (is (not (s/valid? ::p/universal-quantification '(forall [?p] (package ?x) (in ?x ?t))))))
  (testing "::assignment"
    (is (s/valid? ::p/assignment '(def ?test 1)))
    (is (s/valid? ::p/assignment '(def ?test [1])))
    (is (s/valid? ::p/assignment '(def ?test (call + 1 2))))
    (is (not (s/valid? ::p/assignment '(def test 1))))
    (is (not (s/valid? ::p/assignment '(def _test 1))))
    (is (not (s/valid? ::p/assignment '(def !test 1)))))
  (testing "::expression"
    (is (s/valid? ::p/expression '(test ?hello)))
    (is (= :logical-atom
           (first (s/conform ::p/expression '(test ?hello)))))
    (is (s/valid? ::p/expression '?hello))
    (is (= :logical-atom
           (first (s/conform ::p/expression '?hello))))
    (is (s/valid? ::p/expression '(and (hello))))
    (is (= :conjunction
           (first (s/conform ::p/expression '(and (hello))))))
    (is (s/valid? ::p/expression '()))
    (is (= :conjunction
           (first (s/conform ::p/expression '()))))
    (is (s/valid? ::p/expression '(or (hello))))
    (is (= :disjunction
           (first (s/conform ::p/expression '(or (hello))))))
    (is (s/valid? ::p/expression '(not (hello))))
    (is (= :negation
           (first (s/conform ::p/expression '(not (hello))))))
    (is (s/valid? ::p/expression '(imply (sunny ?place) (nice-out ?place))))
    (is (= :implication
           (first (s/conform ::p/expression '(imply (sunny ?place) (nice-out ?place))))))
    (is (s/valid? ::p/expression '(forall [?p] (package ?p) (in ?p ?t))))
    (is (= :universal-quantification
           (first (s/conform ::p/expression '(forall [?p] (package ?p) (in ?p ?t))))))
    (is (s/valid? ::p/expression '(def ?hello 1)))
    (is (= :assignment
           (first (s/conform ::p/expression '(def ?hello 1)))))
    (is (s/valid? ::p/expression '(call >= ?hello 1)))
    (is (= :call
           (first (s/conform ::p/expression '(call >= ?hello 1)))))))

(deftest test-preconditions
  (testing "::first-satisfier-precondition"
    (is (s/valid? ::p/first-satisfier-precondition '(:first (test ?hello))))
    (is (s/valid? ::p/first-satisfier-precondition '(:first ?hello)))
    (is (not (s/valid? ::p/first-satisfier-precondition '(:first))))
    (is (not (s/valid? ::p/first-satisfier-precondition '(:first (test ?hello)
                                                                 (another ?h)))))
    (is (not (s/valid? ::p/first-satisfier-precondition '(:last (test ?hello))))))
  (testing "::sorted-precondition"
    (is (s/valid? ::p/sorted-precondition '(:sort-by ?d > (and (at ?here)
                                                               (distance ?her ?there ?d)))))
    (is (s/valid? ::p/sorted-precondition '(:sort-by ?d > ?test)))
    (is (not (s/valid? ::p/sorted-precondition '(:sort-by ?d >))))
    (is (not (s/valid? ::p/sorted-precondition '(:sort-by ?d))))
    (is (not (s/valid? ::p/sorted-precondition '(:sort-by))))
    (is (not (s/valid? ::p/sorted-precondition '(:first ?d > (and (at ?here)
                                                                  (distance ?her ?there ?d))))))
    (is (not (s/valid? ::p/sorted-precondition '(:sort-by d > (and (at ?here)
                                                                   (distance ?her ?there ?d))))))
    (is (not (s/valid? ::p/sorted-precondition '(:sort-by ?d ?x (and (at ?here)
                                                                     (distance ?her ?there ?d)))))))
  (testing "::logical-precondition"
    (is (s/valid? ::p/logical-precondition '(test ?hello)))
    (is (= :expression
           (first (s/conform ::p/logical-precondition '(test ?hello)))))
    (is (s/valid? ::p/logical-precondition '?hello))
    (is (= :expression
           (first (s/conform ::p/logical-precondition '?hello))))
    (is (s/valid? ::p/logical-precondition '(:first (test ?hello))))
    (is (= :first-satisfier-precondition
           (first (s/conform ::p/logical-precondition '(:first (test ?hello))))))
    (is (s/valid? ::p/logical-precondition '(:sort-by ?d > (and (at ?here)
                                                                (distance ?her ?there ?d)))))
    (is (= :sorted-precondition
           (first (s/conform ::p/logical-precondition '(:sort-by ?d > (and (at ?here)
                                                                           (distance ?her ?there ?d)))))))))

(deftest test-axiom
  (testing "::axiom"
    (is (s/valid? ::p/axiom '(:- (walking-distance ?x)
                                 good
                                 (and (weather-is good)
                                      (distance home ?x ?d)
                                      (call <= ?d 2))
                                 bad
                                 (and (distance home ?x ?d)
                                      (call <= ?d 1)))))
    (= 2
       (count
        (:axioms
         (s/conform ::p/axiom '(:- (walking-distance ?x)
                                   good
                                   (and (weather-is good)
                                        (distance home ?x ?d)
                                        (call <= ?d 2))
                                   bad
                                   (and (distance home ?x ?d)
                                        (call <= ?d 1)))))))
    (is (s/valid? ::p/axiom '(:- (walking-distance ?x)
                                 (and (weather-is good)
                                      (distance home ?x ?d)
                                      (call <= ?d 2))
                                 (and (distance home ?x ?d)
                                      (call <= ?d 1)))))
    (is (s/valid? ::p/axiom '(:- (walking-distance ?x)
                                 good
                                 (and (weather-is good)
                                      (distance home ?x ?d)
                                      (call <= ?d 2))
                                 (and (distance home ?x ?d)
                                      (call <= ?d 1)))))
    (is (= 2
           (count
            (:axioms
             (s/conform ::p/axiom '(:- (walking-distance ?x)
                                       good
                                       (and (weather-is good)
                                            (distance home ?x ?d)
                                            (call <= ?d 2))
                                       (and (distance home ?x ?d)
                                            (call <= ?d 1))))))))
    (is (s/valid? ::p/axiom '(:- (walking-distance ?x)
                                 good
                                 (and (weather-is good)
                                      (distance home ?x ?d)
                                      (call <= ?d 2)))))
    (is (= 1
           (count
            (:axioms
             (s/conform ::p/axiom '(:- (walking-distance ?x)
                                       good
                                       (and (weather-is good)
                                            (distance home ?x ?d)
                                            (call <= ?d 2))))))))
    (is (s/valid? ::p/axiom '(:- (walking-distance ?x))))
    (is (s/valid? ::p/axiom '(:- (walking-distance ?x)
                                 ?good ;; This is ok because it maybe it is supposed to be replaced
                                       ;; Maybe add to a 'Gotchas' section?
                                 (and (weather-is good)
                                      (distance home ?x ?d)
                                      (call <= ?d 2)))))
    (is (s/valid? ::p/axiom '(:- ?test
                                 good
                                 (and (weather-is good)
                                      (distance home ?x ?d)
                                      (call <= ?d 2)))))
    (is (not (s/valid? ::p/axiom '(:- good
                                      (and (weather-is good)
                                           (distance home ?x ?d)
                                           (call <= ?d 2))
                                      bad
                                      (and (distance home ?x ?d)
                                           (call <= ?d 1))))))
    (is (not (s/valid? ::p/axiom '(:- (and (weather-is good)
                                           (distance home ?x ?d)
                                           (call <= ?d 2))
                                      (and (distance home ?x ?d)
                                           (call <= ?d 1))))))
    (is (not (s/valid? ::p/axiom '(:- good))))
    (is (not (s/valid? ::p/axiom '(:- (walking-distance ?x) good))))
    (is (not (s/valid? ::p/axiom '(:- (walking-distance ?x)
                                      !good
                                      (and (weather-is good)
                                           (distance home ?x ?d)
                                           (call <= ?d 2))))))
    (is (not (s/valid? ::p/axiom '(:- (walking-distance ?x)
                                      good
                                      (and (weather-is good)
                                           (distance home ?x ?d)
                                           (call <= ?d 2))
                                      day))))
    (is (not (s/valid? ::p/axiom '(:- (walking-distance ?x)
                                      good
                                      day))))))

(deftest test-task-structures
  (testing "::task-atom"
    (is (s/valid? ::p/task-atom '(!hello 1)))
    (is (s/valid? ::p/task-atom '(:immediate !hello 1)))
    (is (s/valid? ::p/task-atom '(hello 1)))
    (is (s/valid? ::p/task-atom '(:immediate hello 1)))
    (is (s/valid? ::p/task-atom '(!hello)))
    (is (s/valid? ::p/task-atom '(:immediate !hello)))
    (is (s/valid? ::p/task-atom '(hello)))
    (is (s/valid? ::p/task-atom '(:immediate hello)))
    (is (not (s/valid? ::p/task-atom '(:thing !hello 1)))))
  (testing "::task-list"
    (is (s/valid? ::p/task-list '[(!hello 1)]))
    (is (s/valid? ::p/task-list '[(hello 1)]))
    (is (s/valid? ::p/task-list '[(!hello 1) (!hi 2)]))
    (is (s/valid? ::p/task-list '[(hello 1) (!hi 2)]))
    (is (s/valid? ::p/task-list '[[(hello 1)] (!hi 2)]))
    (is (s/valid? ::p/task-list '[(:immediate !hello 1)]))
    (is (s/valid? ::p/task-list '[(:immediate hello 1)]))
    (is (s/valid? ::p/task-list '[:unordered (!hello 1)]))
    (is (s/valid? ::p/task-list '[:unordered (hello 1)]))
    (is (s/valid? ::p/task-list '[:unordered (!hello 1) (!hi 2)]))
    (is (s/valid? ::p/task-list '[:unordered (hello 1) (!hi 2)]))
    (is (s/valid? ::p/task-list '[:unordered (:immediate !hello 1)]))
    (is (s/valid? ::p/task-list '[:unordered (:immediate hello 1)]))
    (is (not (s/valid? ::p/task-list '[:thing (!hello 1) (!hi 2)])))
    (is (not (s/valid? ::p/task-list '[:thing (hello 1) (!hi 2)])))
    (is (not (s/valid? ::p/task-list '[1 (!hi 2)])))
    (is (not (s/valid? ::p/task-list '[1 (hi 2)]))))
  (testing "::task-list"
    (is (s/valid? ::p/task-list '[(!hello 1)]))))

(deftest test-operator
  (testing "::protection-condition"
    (is (s/valid? ::p/protection-condition '(:protection (at ?truck ?location))))
    (is (s/valid? ::p/protection-condition '(:protection (hello (call + 1 2)))))
    (is (s/valid? ::p/protection-condition '(:protection ?hello)))
    (is (not (s/valid? ::p/protection-condition '(:this (at ?truck ?location))))))
  (testing "::forall-expression"
    (is (s/valid? ::p/forall-expression '(forall [?package] (blue ?package) [(red ?package)])))
    (is (not (s/valid? ::p/forall-expression '(forall [?package] (blue ?package) (red ?package)))))
    (is (not (s/valid? ::p/forall-expression '(forall [?package] (blue ?package)))))
    (is (not (s/valid? ::p/forall-expression '(forall [?package]))))
    (is (not (s/valid? ::p/forall-expression '(forall))))
    (is (not (s/valid? ::p/forall-expression '(test [?package] (blue ?package) [(red ?package)])))))
  (testing "::delete-add-element"
    (is (s/valid? ::p/delete-add-element '(at ?truck ?location)))
    (is (= :logical-atom
           (first (s/conform ::p/delete-add-element '(at ?truck ?location)))))
    (is (s/valid? ::p/delete-add-element '?hello))
    (is (= :logical-atom
           (first (s/conform ::p/delete-add-element '?hello))))
    (is (s/valid? ::p/delete-add-element '(:protection (at ?truck ?location))))
    (is (= :protection-condition
           (first (s/conform ::p/delete-add-element '(:protection (at ?truck ?location))))))
    (is (s/valid? ::p/delete-add-element '(:protection (hello (call + 1 2)))))
    (is (s/valid? ::p/delete-add-element '(forall [?package] (blue ?package) [(red ?package)])))
    (is (= :forall-expresssion
           (first (s/conform ::p/delete-add-element '(forall [?package] (blue ?package) [(red ?package)])))))
    (is (not (s/valid? ::p/delete-add-element '(:this (at ?truck ?location)))))
    (is (not (s/valid? ::p/delete-add-element '(imply (at ?truck ?location))))))
  (testing "::delete-add-list"
    (is (s/valid? ::p/delete-add-list '[(at ?truck ?location)
                                        (:protection (at ?truck ?location))
                                        (forall [?package] (blue ?package) (not (red ?package)))]))
    (is (s/valid? ::p/delete-add-list '[?test]))
    (is (s/valid? ::p/delete-add-list '?test))
    (is (= :late-binding
           (first (s/conform ::p/delete-add-list '?test))))
    (is (= :logical-atom
           (first (first (second (s/conform ::p/delete-add-list '[(at ?truck ?location)]))))))
    (is (= :logical-atom
           (first (first (second (s/conform ::p/delete-add-list '[?test]))))))
    (is (= :protection-condition
           (first (first (second (s/conform ::p/delete-add-list '[(:protection (at ?truck ?location))]))))))
    (is (= :universal-quantification
           (first (first (second (s/conform ::p/delete-add-list '[(forall [?package] (blue ?package) (not (red ?package)))])))))))
  (testing "::operator"
    (is (s/valid? ::p/operator '(:operator (!drive-to ?truck ?old-loc ?location)
                                           ()
                                           [(at ?truck ?old-loc)]
                                           [(at ?truck ?location)
                                            (:protection (at ?truck ?location))])))
    (is (s/valid? ::p/operator '(:operator (!pick-up ?truck ?package ?location)
                                           ()
                                           [(at ?package ?location)
                                            (:protection (at ?truck ?location))
                                            (forall [?package] (blue ?package) (not (red ?package)))]
                                           [(in ?package ?truck)])))
    (is (s/valid? ::p/operator '(:operator (!drive-to ?truck ?old-loc ?location)
                                           ()
                                           [(at ?truck ?old-loc)]
                                           [(at ?truck ?location)
                                            (:protection (at ?truck ?location))]
                                           2)))
    (is (s/valid? ::p/operator '(:operator (!drive-to ?truck ?old-loc ?location)
                                           ()
                                           []
                                           [])))
    (is (s/valid? ::p/operator '(:operator (!drive-to ?truck ?old-loc ?location)
                                           ()
                                           ?later
                                           [(at ?truck ?location)
                                            (:protection (at ?truck ?location))])))
    (is (s/valid? ::p/operator '(:operator (!drive-to ?truck ?old-loc ?location)
                                           ()
                                           [(at ?truck ?old-loc)]
                                           ?later)))
    (is (s/valid? ::p/operator '(:operator (!drive-to ?truck ?old-loc ?location)
                                           ?later
                                           [(at ?truck ?old-loc)]
                                           [(at ?truck ?location)
                                            (:protection (at ?truck ?location))])))
    (is (= :normal-task
           (first
            (:head
             (second
              (s/conform ::p/operator '(:operator (!drive-to ?truck ?old-loc ?location)
                                                  ()
                                                  [(at ?truck ?old-loc)]
                                                  [(at ?truck ?location)
                                                   (:protection (at ?truck ?location))])))))))
    (is (s/valid? ::p/operator '(:operator (!drive-to ?truck ?old-loc ?location)
                                           (:first (test ?hello))
                                           [(at ?truck ?old-loc)]
                                           [(at ?truck ?location)
                                            (:protection (at ?truck ?location))])))
    (is (s/valid? ::p/operator '(:operator (!drive-to ?truck ?old-loc ?location)
                                           (test ?hello)
                                           [(at ?truck ?old-loc)]
                                           [(at ?truck ?location)
                                            (:protection (at ?truck ?location))])))
    (is (s/valid? ::p/operator '(:operator (!drive-to ?truck ?old-loc ?location)
                                           (test ?hello)
                                           [(at ?hello ?old-loc)]
                                           [(at ?truck ?location)
                                            (:protection (at ?truck ?location))])))
    (is (not (s/valid? ::p/operator '(:operator (:immediate !drive-to ?truck ?old-loc ?location)
                                                ()
                                                [(at ?truck ?old-loc)]
                                                [(at ?truck ?location)
                                                 (:protection (at ?truck ?location))]))))
    (is (not (s/valid? ::p/operator '(:operator (?drive-to ?truck ?old-loc ?location)
                                                ()
                                                [(at ?truck ?old-loc)]
                                                [(at ?truck ?location)
                                                 (:protection (at ?truck ?location))]))))
    (is (not (s/valid? ::p/operator '(:operator (drive-to ?truck ?old-loc ?location)
                                                ()
                                                [(at ?truck ?old-loc)]
                                                [(at ?truck ?location)
                                                 (:protection (at ?truck ?location))]))))
    (is (not (s/valid? ::p/operator '(:operator (!drive-to ?truck ?old-loc ?location)
                                                ()
                                                [(at ?truck ?old-loc)]
                                                [(at ?truck ?location)
                                                 (:protection (at ?truck ?location))]
                                                2 3))))
    (is (not (s/valid? ::p/operator '(:operator (!drive-to ?truck ?old-loc ?location)
                                                ()
                                                [(at ?a ?old-loc)]
                                                [(at ?truck ?location)
                                                 (:protection (at ?truck ?location))]))))))

(deftest test-method
  (testing "::method"
    (is (s/valid? ::p/method
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
             (s/conform ::p/method
                        '(:method (eat ?food)
                                  branch1
                                  (have-fork ?fork)
                                  [(!eat-with-fork ?food ?fork)]
                                  branch2
                                  (have-spoon ?spoon)
                                  [(!eat-with-spoon ?food ?spoon)]))))))
    (is (s/valid? ::p/method
                  '(:method (eat ?food)
                            branch1
                            (have-fork ?fork)
                            [(!eat-with-fork ?food ?fork)])))
    (is (= 1
           (count
            (:options
             (s/conform ::p/method
                        '(:method (eat ?food)
                                  branch1
                                  (have-fork ?fork)
                                  [(!eat-with-fork ?food ?fork)]))))))
    (is (s/valid? ::p/method
                  '(:method (eat ?food)
                            branch2
                            (and (not (have-fork ?fork))
                                 (have-spoon ?spoon))
                            [(!eat-with-spoon ?food ?spoon)])))
    (is (s/valid? ::p/method
                  '(:method (eat ?food)
                            (and (not (have-fork ?fork))
                                 (have-spoon ?spoon))
                            [(!eat-with-spoon ?food ?spoon)])))
    (is (s/valid? ::p/method
                  '(:method (eat ?food)
                            (have-fork ?fork)
                            [(!eat-with-fork ?food ?fork)]
                            (have-spoon ?spoon)
                            [(!eat-with-spoon ?food ?spoon)])))
    (is (s/valid? ::p/method
                  '(:method (eat ?food)
                            branch1
                            ()
                            [(!eat-with-spoon ?food ?spoon)])))
    (is (s/valid? ::p/method
                  '(:method (eat)
                            ()
                            [(eat-with-spoon)])))
    (is (= 2
           (count
            (:options
             (s/conform ::p/method
                        '(:method (eat ?food)
                                  (have-fork ?fork)
                                  [(!eat-with-fork ?food ?fork)]
                                  (have-spoon ?spoon)
                                  [(!eat-with-spoon ?food ?spoon)]))))))
    (is (s/valid? ::p/method
                  '(:method (eat ?food)
                            branch1
                            (have-fork ?fork)
                            [(!eat-with-fork ?food ?fork)]
                            (have-spoon ?spoon)
                            [(!eat-with-spoon ?food ?spoon)])))
    (is (= 2
           (count
            (:options
             (s/conform ::p/method
                        '(:method (eat ?food)
                                  branch1
                                  (have-fork ?fork)
                                  [(!eat-with-fork ?food ?fork)]
                                  (have-spoon ?spoon)
                                  [(!eat-with-spoon ?food ?spoon)]))))))
    (is (not (s/valid? ::p/method
                       '(:method (!eat ?food)
                                 branch1
                                 (have-fork ?fork)
                                 [(!eat-with-fork ?food ?fork)]
                                 branch2
                                 (have-spoon ?spoon)
                                 [(!eat-with-spoon ?food ?spoon)]))))
    (is (not (s/valid? ::p/method
                       '(:method (?eat ?food)
                                 branch1
                                 (have-fork ?fork)
                                 [(!eat-with-fork ?food ?fork)]
                                 branch2
                                 (have-spoon ?spoon)
                                 [(!eat-with-spoon ?food ?spoon)]))))
    (is (not (s/valid? ::p/method
                       '(:method (eat ?food)
                                 not
                                 (have-fork ?fork)
                                 [(!eat-with-fork ?food ?fork)]
                                 branch2
                                 (have-spoon ?spoon)
                                 [(!eat-with-spoon ?food ?spoon)]))))
    (is (not (s/valid? ::p/method
                       '(:method (eat ?food)
                                 branch1
                                 (!have-fork ?fork)
                                 [(!eat-with-fork ?food ?fork)]
                                 branch2
                                 (have-spoon ?spoon)
                                 [(!eat-with-spoon ?food ?spoon)]))))))

(deftest test-domain-extension
  (testing "::domain-extension"
    (is (s/valid? ::p/domain-extension '[(:method (have-breakfast)
                                                  ()
                                                  [(choose-drink) (make-meal) (!eat)])]))
    (is (s/valid? ::p/domain-extension '[(:method (have-breakfast)
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
                                                  [(!make-eggs ?eggs)])]))
    (is (s/valid? ::p/domain-extension '[(:operator (!drive-to ?truck ?old-loc ?location)
                                                    ()
                                                    [(at ?truck ?old-loc)]
                                                    [(at ?truck ?location)
                                                     (:protection (at ?truck ?location))])]))
    (is (s/valid? ::p/domain-extension '[(:- (walking-distance ?x)
                                             good
                                             (and (weather-is good)
                                                  (distance home ?x ?d)
                                                  (call <= ?d 2))
                                             bad
                                             (and (distance home ?x ?d)
                                                  (call <= ?d 1)))]))
    (is (s/valid? ::p/domain-extension '[(:method (have-breakfast)
                                                  ()
                                                  [(choose-drink) (make-meal) (!eat)])
                                         (:operator (!drive-to ?truck ?old-loc ?location)
                                                    ()
                                                    [(at ?truck ?old-loc)]
                                                    [(at ?truck ?location)
                                                     (:protection (at ?truck ?location))])
                                         (:- (walking-distance ?x)
                                             good
                                             (and (weather-is good)
                                                  (distance home ?x ?d)
                                                  (call <= ?d 2))
                                             bad
                                             (and (distance home ?x ?d)
                                                  (call <= ?d 1)))]))
    (is (s/valid? ::p/domain-extension '[(:method (have-breakfast)
                                                  ()
                                                  [(choose-drink) (make-meal) (!eat)])
                                         (:operator (!drive-to ?truck ?old-loc ?location)
                                                    ()
                                                    [(at ?truck ?old-loc)]
                                                    [(at ?truck ?location)
                                                     (:protection (at ?truck ?location))])
                                         (:- (walking-distance ?x)
                                             good
                                             (and (weather-is good)
                                                  (distance home ?x ?d)
                                                  (call <= ?d 2))
                                             bad
                                             (and (distance home ?x ?d)
                                                  (call <= ?d 1)))]))
    (is (not (s/valid? ::p/domain-extension '[(choose-drink)])))
    (is (not (s/valid? ::p/domain-extension '[(!choose-drink)])))
    (is (not (s/valid? ::p/domain-extension '[(:test (something))])))))

(deftest test-problem
  (testing "::state-list"
    (is (s/valid? ::p/state-list '[(hello 1) (test) (hi 1 constant 3)]))
    (is (s/valid? ::p/state-list '[]))
    (is (s/valid? ::p/state-list '[(hello (call + 1 2))]))
    (is (not (s/valid? ::p/state-list '[(!hello)]))))
  (testing "::problem"
    (is (s/valid? ::p/problem '(defproblem
                                 [(hello 1)]
                                 [(!test) (eat) (!sleep)])))
    (is (s/valid? ::p/problem '(defproblem
                                 []
                                 [])))
    (is (not (s/valid? ::p/problem '(defproblem test
                                      [(hello 1)]
                                      [(!test) (eat) (!sleep)]))))
    (is (not (s/valid? ::p/problem '(defproblem
                                      [(hello 1) (hello ?test)]
                                      [(!test) (eat) (!sleep)]))))
    (is (not (s/valid? ::p/problem '(:problem test
                                              [(hello 1)]
                                              [(!test) (eat) (!sleep)]))))
    (is (not (s/valid? ::p/problem '(defproblem !test
                                      [(hello 1)]
                                      [(!test) (eat) (!sleep)]))))
    (is (not (s/valid? ::p/problem '(defproblem ?test
                                      [(hello 1)]
                                      [(!test) (eat) (!sleep)]))))
    (is (not (s/valid? ::p/problem '(defproblem test
                                      [(!test) (eat) (!sleep)]))))
    (is (not (s/valid? ::p/problem '(defproblem
                                      [(hello 1)]))))
    (is (not (s/valid? ::p/problem '(defproblem))))))

(deftest test-parse-domain-extension
  (testing "parse-domain-extension"
    (is (not (nil? (p/parse-domain-extension '[(:- (hello))]))))
    (is (not (nil? (p/parse-domain-extension "[(:- (hello))]"))))
    (is (not (nil? (p/parse-domain-extension "[(:- ;; This is a test of comments
                                                  (hello))]"))))
    (is (not (nil? (p/parse-domain-extension '[(:method (hello ?test) () [(!test) (thing)])]))))
    (is (not (nil? (p/parse-domain-extension "[(:method (hello ?test) () [(!test) (thing)])]"))))
    (is (not (nil? (p/parse-domain-extension '[(:operator (!drive-to ?truck ?old-loc ?location)
                                                          ()
                                                          [(at ?truck ?old-loc)]
                                                          [(at ?truck ?location)
                                                           (:protection (at ?truck ?location))])]))))
    (is (not (nil? (p/parse-domain-extension "[(:operator (!drive-to ?truck ?old-loc ?location)
                                                          ()
                                                          [(at ?truck ?old-loc)]
                                                          [(at ?truck ?location)
                                                            (:protection (at ?truck ?location))])]"))))
    (is (nil? (p/parse-domain-extension "[(:- (hello))")))
    (is (nil? (p/parse-domain-extension "[(:- (hello)]")))))

(deftest test-parse-problem
  (testing "parse-problem"
    (let [prob (p/parse-problem '(defproblem [(hello 1)] [(!test)]))]
      (is (not (or (nil? prob)
                   (= :clojure.spec.alpha/invalid prob)))))
    (let [prob (p/parse-problem "(defproblem [(hello 1)] [(!test)])")]
      (is (not (or (nil? prob)
                   (= :clojure.spec.alpha/invalid prob)))))
    (let [prob (p/parse-problem "(defproblem ;; This is a test of comments
                                                 [(hello 1)] [(!test)])")]
      (is (not (or (nil? prob)
                   (= :clojure.spec.alpha/invalid prob)))))
    (let [prob (p/parse-problem "(defproblem [(hello 1] [(!test)])")]
      (is (or (nil? prob)
              (= :clojure.spec.alpha/invalid prob))))
    (let [prob (p/parse-problem "(defproblem [(hello 1)] [(!test))")]
      (is (or (nil? prob)
              (= :clojure.spec.alpha/invalid prob))))))
