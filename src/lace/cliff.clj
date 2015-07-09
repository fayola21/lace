(ns lace.cliff
  (:use (incanter core stats))
  (:use (lace utils)))


(declare select-numeric-instances)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLIFF FOR NUMERIC DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn ncliff [data & {:keys [n q]
                      :or {n 10 q 0.2}}]
  "
  Returns a pruned list of instances with numeric independent values.

  Arguments:
    data -- original data

  Options:
    :n (default 10), number of bins for equal frequency binning (efb2)
    :q (default 0.2), portion of instances selected 
  
  References:
    http://spare.lero.ie/pdf/lace2.pdf 

  "
  (let [klass (sel data :cols (dec (ncol data)))
        ind-vars (sel data :cols (range (dec (ncol data))))
        discretized-data (bind-columns
                           (trans (efb2 ind-vars n))
                           klass)
        cliff-data (if (= q 1.0) ; discretized
                     discretized-data
                     (distinct
                       (select-numeric-instances
                         discretized-data
                         q)))
        match-data (fn [dat] ; from discretized to numeric
                     (loop [m 0
                            dd discretized-data
                            d data
                            result (transient [])]
                       (if (empty? d)
                         (persistent! result)
                         (recur
                           (inc m)
                           (rest dd)
                           (rest d)
                           (conj! result (if (= dat (first dd))
                                           (first d)
                                           nil)))))) 
        result (apply concat (map #(match-data %) cliff-data))]  
    (matrix (distinct (remove #(= nil %) result)))))


(defn mygroup [data]
  (let [goals (map first (frequencies (sel data :cols (- (ncol data) 1))))] 
    (loop [g goals result (transient [])]
      (if (empty? g)
        (persistent! result)
        (recur
          (rest g)
          (conj! result
                 (apply
                   vector
                   (filter #(= (last %)
                               (first g)) (to-vect data)))))))))


(defn my-best-rest [data]
  "data = binned version"
  (let [group-it (mygroup data)]
    (loop [g group-it results (transient [])]
      (if (empty? g)
        (persistent! results)
        (recur (rest g)
               (conj! 
                 results 
                 (vector (first g) 
                         (matrix 
                           (apply 
                             concat 
                             (filter #(not=
                                        (first g)
                                        %)
                                     group-it))))))))))


(defn get-criteria-only [D]
  "Returns criteria for the dataset D. Used for
   numeric data."  
  (let [br (my-best-rest D)]
    (loop [br1 br result (transient [])]
      (if (empty? br1)
        (persistent! result)
        (recur (rest br1)
               (conj! 
                 result 
                 (get-ranks-only D 
                                 (first
                                   (first br1))
                                 (second (first br1)))))))))


(defn calculate-inst-rank [one crit0]
  "Returns the rank value of an instance by multipling the individual
   ranks of each attribute value in the instance."
  (loop [n 0 result (transient [])]
    (if (= n (count one))
      (apply * (map second (apply concat (persistent! result))))
      (recur
        (inc n)
        (conj! result (let [get1 (nth crit0 n)
                            get2 (filter #(= (nth one n)
                                             (first %)) get1)]
                        get2))))))


(defn rank-instances [D]
  (let [crit (get-criteria-only D)
        inst-group (mygroup D)        
        inst (loop [dat inst-group c crit result (transient [])]
               (if (empty? dat)
                 (map #(reverse (sort-by second %)) (persistent! result))
                 (recur
                   (rest dat)
                   (rest crit)
                   (conj! result
                          (map #(vector %
                                        (calculate-inst-rank
                                          (butlast %)
                                          (first crit)))
                               (first dat))))))]
    inst))


(defn select-numeric-instances [D q]
  (let [r-instances (rank-instances D)
        get-r (loop [rinst r-instances result (transient [])]
                (if (empty? rinst)
                  (map first (apply concat (persistent! result)))
                  (recur
                    (rest rinst)
                    (conj! result
                           (take (Math/ceil
                                   (* q (count (first rinst))))
                                 (first rinst))))))]
    get-r))   