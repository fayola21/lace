(ns lace.core
  (:use [clojure.tools.cli :only (cli)])
  (:use (incanter core stats))
  (:use (lace cliff ipr leaf morph utils))
  (:gen-class))


(defn cliffmorph [data  & {:keys [sav n q m distance-fnc]
                           :or {sav [10 20] n 10 q 0.2 m 0.4 distance-fnc euclidean-distance}}]
  "
  Applying CLIFF and MORPH to a data set.
  "
  (let [data1 (matrix (distinct (to-vect data)))
        cliff-data (ncliff data1 :q q)
        morph-cliff (morph cliff-data :sav sav :m m :distance-fnc distance-fnc)]
    morph-cliff))


(defn demo-ipr [data]
  "
  Finding IPR of obfuscated data set.
  "
  (iprmult data (cliffmorph data)))


(defn -main [& args]
  (let [[opts args banner]
        (cli args
             ["-h" "--help" "Show help" :flag true :default false]
             ["-d" "--data" "Data to be obfuscated"]
             ["--ipr" "Increased Privacy Ration" :flag true :default false]
             ["--cm" "CLIFF+MORPH" :flag true :default false])]
    (when (:help opts)
      (println banner)
      (System/exit 0))
    (if (:data opts)
      (cond
      (:ipr opts) (println (demo-ipr (change-to-matrix (:data opts))))
      (:cm opts) (println (cliffmorph (change-to-matrix (:data opts))))
      banner (System/exit 0))
      (println banner))))



