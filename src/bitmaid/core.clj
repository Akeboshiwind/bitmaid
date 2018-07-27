(ns bitmaid.core
  (:gen-class)
  (:require [bitmaid.domain.compiler :as c]
            [bitmaid.domain.executor :as e]
            [bitmaid.jshop-wrapper :refer :all]
            [clojure.java.shell :refer [sh]]
            [clojure.pprint :refer [pprint]]))

(defn re-files
  ([re]
   (re-files re "."))
  ([re dir]
   (let [files (->> (clojure.java.io/file dir)
                    (.listFiles)
                    (filter #(.isFile %))
                    (filter #(re-matches re (.getName %))))]
     (if (empty? files)
       nil
       files))))

(defn delete-files
  [files]
  (doseq [f files]
    (clojure.java.io/delete-file f)))

(defn generate-plan
  [domain problem]
  (println "Create domain file")
  (spit "housedomain" (with-out-str (pprint (e/encode domain))))

  (println "Create problem file")
  (spit "problem" (with-out-str (pprint (e/encode problem))))

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

    (println "Shutting down agents")
    (shutdown-agents)

    (println "Finished")
    (parse-plan-text (:out plan))))

(defn file->dom-ext
  [file]
  (-> file
      (slurp)
      (c/precompile-domain-extension)))

(defn file->problem
  [file]
  (-> file
      (slurp)
      (c/precompile-problem)))

(defn extend-domain
  [base extension]
  (-> base
      (update :axioms (partial merge (:axioms extension)))
      (update :methods (partial merge (:methods extension)))
      (update :operators (partial merge (:operators extension)))))

(comment
  (def domain
    (->> (re-files #".*\.dext")
         (map file->dom-ext)
         (reduce (fn
                   ([] nil)
                   ([& args] (apply extend-domain args))))))

  (def problem
    (->> (re-files #".*\.prob")
         (first)
         (file->problem))))

(defn -main
  [& args]
  (println "Searching for domain extension files")
  (if-let [domain-files (re-files #".*\.dext")]
    (do
      (println "Found domain extensions:")
      (doseq [f domain-files]
        (println (.getName f)))

      (if-let [problem-files (re-files #".*\.prob")]
        (let [problem-file (first problem-files)]
          (println "Found a problem file:")
          (println (.getName problem-file))

          (println "Compiling domain extensions")
          (if-let [domain (some->> domain-files
                                   (map file->dom-ext)     ;; Convert the files to internal representation
                                   (reduce (fn             ;; Merge extensions into one domain
                                             ([] nil)
                                             ([& args] (apply extend-domain args)))))]
            (do
              (println "Compiling problem")
              (if-let [problem (file->problem problem-file)];; Convert file to internal representation
                (do
                  (println "Generating plan")
                  (if-let [plan (generate-plan domain problem)]
                    (do
                      (println)
                      (println "Plan found!")
                      (println (str "Plan: " (:plan plan)))
                      (println (str "Cost: " (:cost plan))))
                    (do
                      (println)
                      (println "No plans were found"))))
                (println "Failed to compile problem")))
            (println "Failed to compile domain extensions")))
        (println "No problem file found")))
    (println "No domain extensions found")))
