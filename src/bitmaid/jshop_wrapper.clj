(ns bitmaid.jshop-wrapper)

(defn- main
  [& args]
  (JSHOP2.InternalDomain/main (into-array String args)))

(defn gen-domain
  [file-name]
  (main file-name))

(defn gen-plan
  [file-name]
  (main "-r" file-name))
