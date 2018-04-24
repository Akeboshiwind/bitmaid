(ns bitmaid.core
  (:require [cljs.nodejs :as nodejs]
            [bitmaid.domain.compiler :as c]
            [bitmaid.domain.executor :as e]))

(nodejs/enable-util-print!)

(def current-domain (atom (c/precompile-domain-extension "[]")))

(merge {:a {:b 1} :b {:a 1}}
       {:a {:a 2} :b {:b 2}})

(def dom (c/precompile-domain-extension "[(:- (hello ?test))]"))

(defn extend-domain
  [base extension]
  (-> base
      (update :axioms (partial merge (:axioms extension)))
      (update :methods (partial merge (:methods extension)))
      (update :operators (partial merge (:operators extension)))))

(defn -main
  []
  (println "Hello world!"))

 ;;; TODO ;;;
;; Execute tasks in order
;; Decompose tasks then execute
;; Import task list from API
;; Plugins to import tasks to be executed
;; Plugins to import middleware tasks
;; Support definition of main-tasks from user API
;; Similar to middleware tasks

(set! *main-cli-fn* -main)
