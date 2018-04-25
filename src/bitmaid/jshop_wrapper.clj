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
    (when (re-matches #"[1-9][0-9]* plan\(s\) were found:" (second lines))
      (let [cost-str (re-find #"\d+\.\d+" (nth lines 4))]
        {:plan (find-plan lines 6)
         :cost (read-string cost-str)}))))
