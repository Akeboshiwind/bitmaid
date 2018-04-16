(ns bitmaid.domain.parser
  (:require [cljs.spec.alpha :as s]
            [cljs.spec.gen.alpha :as gen]
            [cljs.reader :refer [read-string]]
            [clojure.set :refer [union subset?]]
            [expound.alpha :refer [expound]]))

(s/def ::variable-symbol
  (s/and symbol?
         (comp #{\?} first name)))

(s/def ::task-symbol
  (s/and symbol?
         (comp #{\!} first name)))

(def restricted-symbols
  #{'not 'call 'and 'or 'imply 'forall 'def})

(defn restricted?
  [symbol]
  (boolean (restricted-symbols symbol)))

(s/def ::general-symbol
  (s/and (some-fn keyword? symbol?)
         (comp not #{\? \!} first name)
         (comp not restricted?)))

(s/def ::constant-symbol ::general-symbol)
(s/def ::predicate-symbol ::general-symbol)
(s/def ::compound-task-symbol ::general-symbol)

(s/def ::function-symbol ::general-symbol)

(s/def ::list nil)
(s/def ::call nil)

(s/def ::term
  (s/or
   :variable-symbol ::variable-symbol
   :constant-symbol ::constant-symbol
   :number   number?
   :call     ::call
   :list     ::list))

(s/def ::list
  (s/and vector?
         (s/coll-of ::term)))

(s/def ::call
  (s/and list?
         (s/cat :call #{'call}
                :function-symbol ::function-symbol
                :terms (s/* ::term))))

(s/def ::logical-atom
  (s/and list?
         (s/cat :predicate-symbol ::predicate-symbol
                :terms (s/* ::term))))

(s/def ::conjunction nil)
(s/def ::disjunction nil)
(s/def ::negation nil)
(s/def ::implication nil)
(s/def ::universal-quantification nil)
(s/def ::assignment nil)

(s/def ::expression
  (s/or :conjunction  ::conjunction
        :disjunction  ::disjunction
        :negation     ::negation
        :implication  ::implication
        :universal-quantification ::universal-quantification
        :assignment   ::assignment
        :call         ::call
        :logical-atom ::logical-atom))

(s/def ::conjunction
  (s/or :empty (every-pred list? empty?)
        :normal (s/cat :and #{'and}
                       :expressions (s/* ::expression))))

(s/def ::disjunction
  (s/cat :or #{'or}
         :expressions (s/* ::expression)))

(s/def ::negation
  (s/cat :not #{'not}
         :expression ::expression))

(s/def ::implication
  (s/cat :imply #{'imply}
         :lhs ::expression
         :rhs ::expression))

(defn walk-tree
  "Returns a sequence of the nodes in a tree, via depth-first walk."
  [tree]
  (tree-seq seqable? identity tree))

(defn find-variables
  "Returns a set of the variables found in a spec form."
  [spec-form]
  (->> (walk-tree spec-form)
       (filter #(and (seqable? %)
                     (= :variable-symbol (first %))))
       (map second)
       set))

(defn set=
  "Returns true if all vectors input contain the same elements."
  [& vectors]
  (apply = (map set vectors)))

(s/def ::universal-quantification
  (s/and
   (s/cat :forall #{'forall}
          :variable-symbols (s/and vector?
                                   (s/coll-of ::variable-symbol))
          :predicate ::expression
          :form ::expression)
   (fn [form]
     (let [vars (:variable-symbols form)]
       (set= vars (find-variables (:predicate form)))))))

(s/def ::assignment
  (s/cat :def #{'def}
         :name ::variable-symbol
         :value ::term))

(s/def ::first-satisfier-precondition
  (s/cat :first #{:first}
         :expression ::expression))

(s/def ::sorted-precondition
  (s/cat :first #{:sort-by}
         :variable-symbol ::variable-symbol
         :comparitor ::function-symbol
         :expression ::expression))

(s/def ::logical-precondition
  (s/or :first-satisfier-precondition ::first-satisfier-precondition
        :sorted-precondition ::sorted-precondition
        :expression ::expression))

(s/def ::axiom
  (s/cat :axiom #{':-}
         :head ::logical-atom
         :axioms (s/* (s/alt :named (s/cat :name ::general-symbol
                                           :logical-precondition ::logical-precondition)
                             :unnamed (s/cat :logical-precondition ::logical-precondition)))))

(s/def ::normal-task-atom
  (s/cat :name ::task-symbol
         :terms (s/* ::term)))

(s/def ::normal-compound-task-atom
  (s/cat :name ::compound-task-symbol
         :terms (s/* ::term)))

(s/def ::task-atom*
  (s/alt :primitive ::normal-task-atom
         :compound ::normal-compound-task-atom))

(s/def ::task-atom
  (s/or :immediate-task (s/cat :immediate #{':immediate}
                               :task ::task-atom*)
        :normal-task ::task-atom*))

(s/def ::task-list nil)

(s/def ::task-list-element
  (s/or :task-atom ::task-atom
        :task-list ::task-list))

(s/def ::task-list*
  (s/* ::task-list-element))

(s/def ::task-list
  (s/and vector?
         (s/alt :unordered (s/cat :unordered #{':unordered}
                                  :task-lists ::task-list*)
                :ordered (s/cat :task-lists ::task-list*))))

(s/def ::protection-condition
  (s/cat :protection #{':protection}
         :logical-atom ::logical-atom))

(s/def ::delete-add-element
  (s/or :logical-atom ::logical-atom
        :protection-condition ::protection-condition
        :forall ::universal-quantification))

(s/def ::delete-add-list
  (s/and vector?
         (s/coll-of ::delete-add-element)))

(s/def ::operator
  (s/&
   (s/alt :set-cost (s/cat :operator #{':operator}
                           :head (s/or :normal-task ::normal-task-atom)
                           :precondition ::logical-precondition
                           :delete-list (s/and vector? (s/coll-of ::delete-add-element))
                           :add-list (s/and vector? (s/coll-of ::delete-add-element))
                           :cost number?)
          :no-cost  (s/cat :operator #{':operator}
                           :head (s/or :normal-task ::normal-task-atom)
                           :precondition ::logical-precondition
                           :delete-list (s/and vector? (s/coll-of ::delete-add-element))
                           :add-list (s/and vector? (s/coll-of ::delete-add-element))))
   (fn vars-check [op]
     (let [op (second op)
           head-vars (find-variables (:head op))
           precond-vars (find-variables (:precondition op))
           delete-list-vars (find-variables (:delete-list op))
           add-list-vars (find-variables (:add-list op))
           legal-vars (union head-vars precond-vars)
           check-vars (union delete-list-vars add-list-vars)]
       (subset? check-vars legal-vars)))))

(s/def ::method
  (s/cat :method #{':method}
         :head (s/or :compound-task ::normal-compound-task-atom)
         :options (s/* (s/alt :named (s/cat :name ::general-symbol
                                            :precondition ::logical-precondition
                                            :tail ::task-list)
                              :unnamed (s/cat :precondition ::logical-precondition
                                              :tail ::task-list)))))

(s/def ::domain-extension
  (s/coll-of (s/alt :method ::method
                    :operator ::operator
                    :axiom ::axiom)))

(def ground? (comp empty? find-variables))

(s/def ::problem
  (s/&
   (s/cat :defproblem #{'defproblem}
          :name ::general-symbol
          :initial-state (s/and vector?
                                (s/* ::logical-atom))
          :task-list ::task-list)
   (comp ground? :initial-state)))

(defn parse-spec
  "Takes a spec and a form to be parsed or a string containing a form to be parsed,
  attempts to parse it.
  If the form is invalid of produces and error while parsing, returns nil."
  [spec form]
  (try
    (let [form (if (string? form) (read-string form) form)
          parsed (s/conform spec form)]
      (if (not (= :cljs.spec.alpha/invalid parsed))
        parsed
        (do
          (println "Failed to parse input form.")
          (expound spec form))))
    (catch :default e (println "Parsing form caused an exception."))))


(defn parse-domain-extension
  "Takes a `domain-extension` form, or a string containing the form, and attempts to parse it.
  If the form is invalid or produces an error while parsing returns nil."
  [form]
  (parse-spec ::domain-extension form))

(defn parse-problem
  "Takes a `problem` form, or a string containing the form, and attempts to parse it.
  If the form is invalid or produces an error while parsing returns nil."
  [form]
  (parse-spec ::problem form))
