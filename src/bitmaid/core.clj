(ns bitmaid.core
  (:require [bitmaid.domain.compiler :as c]
            [bitmaid.domain.executor :as e]
            [bitmaid.jshop-wrapper :refer [gen-domain gen-plan]]
            [clojure.java.shell :refer [sh]]
            [clojure.pprint :refer [pprint]]))

;;; TODO ;;;
;; Execute tasks in order
;; Decompose tasks then execute
;; Import task list from API
;; Plugins to import tasks to be executed
;; Plugins to import middleware tasks
;; Support definition of main-tasks from user API
;; Similar to middleware tasks

(def current-domain (atom (c/precompile-domain-extension "[]")))

(defn extend-domain
  [base extension]
  (-> base
      (update :axioms (partial merge (:axioms extension)))
      (update :methods (partial merge (:methods extension)))
      (update :operators (partial merge (:operators extension)))))

(def domain (c/precompile-domain-extension
             "[(:method (swap ?x ?y)
                  (and (have ?x) (not (have ?y)))
                  [(!drop ?x) (!pickup ?y)]
                  (and (have ?y) (not (have ?x)))
                  [(!drop ?y) (!pickup ?x)])
                 (:operator (!drop ?a) (and (have ?a)) [(have ?a)] [])
                 (:operator (!pickup ?a) () [] [(have ?a)])]"))

(def problem (c/precompile-problem
              "(defproblem problem
                   [(have kiwi)]
                   [(swap banjo kiwi)])"))

(defn find-plan
  [lines start]
  (let [length (count lines)]
    (loop [pos start
           acc []]
      (let [current (nth lines pos)]
        (if (or (not (= \( (first current)))
                (>= pos length))
          acc
          (recur (+ pos 1)
                 (conj acc current)))))))

(defn parse-plan-text
  [text]
  (let [lines (clojure.string/split-lines text)]
    (when (re-matches #".*plan\(s\) were found:" (second lines))
      (let [cost-str (re-find #"\d+\.\d+" (nth lines 4))]
        {:plan (find-plan lines 6)
         :cost (read-string cost-str)}))))

(defn re-files
  ([re]
   (re-files re "."))
  ([re dir]
   (->> (clojure.java.io/file dir)
        (.listFiles)
        (filter #(.isFile %))
        (filter #(re-matches re (.getName %))))))

(defn delete-files
  [files]
  (doseq [f files]
    (clojure.java.io/delete-file f)))

(defn generate-plan
  [domain problem]
  (println "Create domain file")
  (spit "housedomain" (with-out-str (pprint (e/compile domain))))

  (println "Create problem file")
  (spit "problem" (with-out-str (pprint (e/compile problem))))

  (println "Compile domain")
  (gen-domain "housedomain")

  (println "Compile problem")
  (gen-plan "problem")

  (println "Recompile problem")
  (sh "javac"
      "-classpath" ".:resources/JSHOP2.jar:resources/antlr.jar"
      "problem.java")

  (println "Plan")
  (let [plan (sh "java"
                 "-classpath" ".:resources/JSHOP2.jar"
                 "problem")]

    (println "Delete class files")
    (delete-files (re-files #".*\.class"))

    (println "Delete java files")
    (delete-files (re-files #".*\.java"))

    (println "Delete domain files")
    (delete-files (re-files #"housedomain.*"))

    (println "Delete problem file")
    (delete-files (re-files #"problem"))

    (parse-plan-text (:out plan))))

(defn -main
  []
  #_
  (do
    (read-domain-files)
    (read-problem-file)
    (generate-plan @domain problem)))
