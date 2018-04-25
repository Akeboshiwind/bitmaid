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
(comment
  (def domain (some->> (re-files #".*\.dext") ;; Read the domain files
                       (map file->dom-ext)    ;; Convert the files to internal representation
                       (reduce (fn            ;; Merge extensions into one domain
                                 ([] nil)
                                 ([& args] (apply extend-domain args))))))

  (def problem (some->> (re-files #".*\.prob") ;; Read problem file
                        (first)
                        (file->problem))))       ;; Convert file to internal representation

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

    (println "Finished")
    (println (parse-plan-text (:out plan)))))

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

(defn -main
  [& args]
  (let [domain (some->> (re-files #".*\.dext") ;; Read the domain files
                        (map file->dom-ext)    ;; Convert the files to internal representation
                        (reduce (fn            ;; Merge extensions into one domain
                                  ([] nil)
                                  ([& args] (apply extend-domain args)))))

        problem (some->> (re-files #".*\.prob") ;; Read problem file
                         (first)
                         (file->problem))]       ;; Convert file to internal representation
    (if (not (nil? domain))
      (if (not (nil? problem))
        (if-let [plan (generate-plan domain problem)]
          plan
          (println "No plans were found"))
        (println "No problem file found"))
      (println "No domain file(s) found"))))
