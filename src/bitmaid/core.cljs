(ns bitmaid.core
  (:require [cljs.nodejs :as nodejs]))

(nodejs/enable-util-print!)

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
