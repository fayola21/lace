(ns lace.lace2
  (:use (incanter core stats))
  (:use (lace cliff ipr leaf morph utils)))


(use 'lace.data)
;;(use 'lace.lace2)


;; Train Data, proprietary.
(def lace-trains-keys [:p1v192 :p2v276 :p3v318 :p4v362 :p42v454 :p43v512 :p5v185])
(def lace-trains-vals [p1v192 p2v276 p3v318 p4v362 p42v454 p43v512 p5v185])
(def lace-trains-hash (zipmap lace-trains-keys lace-trains-vals))


;; Test Data, open source, initiators.
(def lace-tests-keys [:ant17z :camel16z :ivy20z :jedit41z :lucene24z :poi30z :synapse12z :velocity161z :xalan26z :xerces13z])
(def lace-tests-vals [ant17z camel16z ivy20z jedit41z lucene24z poi30z synapse12z velocity161z xalan26z xerces13z])
(def lace-tests-hash (zipmap lace-tests-keys lace-tests-vals))


(defn cliffmorph [data1  & {:keys [sav n q m distance-fnc]
                              :or {sav [10 20] n 10 q 0.2 m 0.4 distance-fnc euclidean-distance}}]
  (let [data (matrix (distinct (to-vect data1)))
        cliff-data (ncliff data :q q)
        morph-cliff (morph cliff-data :sav sav :m m :distance-fnc distance-fnc)]
    morph-cliff))


(defn nn-self [train test & {:keys [distance-fnc]
                             :or {distance-fnc euclidean-distance}}]
  (* 100.0 
     (/ (apply + (map #(if (= (last %) (last (first (get-nearest1 % train distance-fnc)))) 1 0) test))
        (count test))))

(defn crit [cliff-data priv ip ac sav m q cliff-fnc z dist-s]
  (loop [p priv 
         m (sample [0.15 0.2 0.25 0.3 0.35] :size 1)
         q (sample [0.2]  :size 1)]         
    (if (and
          (>= (iprmult cliff-data p :query-size 1 :sav sav :worst false) ip)
          (>= (nn-self p cliff-data) ac))
      [m q p]
      (recur
        (let [exemplars-result  (leaf2 (cliff-fnc cliff-data :m m :q q :sav sav) dist-s :only-exemplars false :p z)
              initial-exemplars (morph (first exemplars-result) :sav sav :m m)]
          initial-exemplars)
        (sample [0.15 0.2 0.25 0.3 0.35] :size 1)
        (sample [0.2] :size 1)))))

(defn crit-rest [cliff-data priv ip ac sav]
  (if (and
        (>= (iprmult cliff-data priv :query-size 1 :sav sav :worst false) ip)
        (>= (nn-self priv cliff-data) ac))
    true
    false))



(defn lace2
  "
  
  Return morphed exemplars. Creates the private cache.
  
  Example:
    (lace :lace :cliff-fnc ncliff :exp 'exp4')

  "
  [model-key 
   sensitive-attribute-indices 
   bins? 
   nosav 
   run & {:keys [bug p m q n query-size data-keys data-hash cliff-fnc worst ip ac]
          :or {bug 20 p 0.1 m 0.4 q 0.2 n 10 
               query-size 1 data-keys (shuffle lace-trains-keys) 
               data-hash lace-trains-hash cliff-fnc cliffmorph worst false ip 80 ac 80}}]
  (let [sav               (conj sensitive-attribute-indices bug)
        original-data     ((first data-keys) data-hash)
        train0   (fn [dat]
                   (let [mybin (bind-columns (trans (butlast (efb2 dat n))) (sel dat :cols (dec (ncol dat))))
                         m1    (to-vect (trans mybin))]
                     (loop [i 0 result []]
                       (if (= i (ncol dat))
                         (trans result)
                         (recur
                           (inc i)
                           (conj result (if (and (member? i (butlast sav))
                                                 ((zipmap sensitive-attribute-indices bins?) i))
                                          (nth m1 i)
                                          (nth (trans dat) i))))))))
        cliff-data (train0 original-data)
        
        exemplars-result  (leaf1 (cliff-fnc cliff-data :m m :q q :sav sav) :only-exemplars false :p p)
        dist-s            (second exemplars-result)
        best-initial      (crit cliff-data (morph (first exemplars-result) :sav sav :m m) ip ac sav m q cliff-fnc p dist-s)
        initial-exemplars (to-list (last best-initial))]    
    (println (str "lace " 1 " done"))
    (loop [i 2 
           dat (rest data-keys) 
           X   (loop [mm (sample [0.15 0.2 0.25 0.3 0.35] :size 1)
                      cq (sample [0.2] :size 1)]
                      
                 (let [ans (follow-the-leader 
                             initial-exemplars                 
                             (cliff-fnc (train0 ((second data-keys) data-hash)) :m mm :q cq :sav sav) 
                             dist-s mm)]
                       (if (crit-rest (train0 ((second data-keys) data-hash))
                                      (matrix (second ans))
                                      ip ac sav)
                         ans
                         (recur
                           (sample [0.15 0.2 0.25 0.3 0.35] :size 1)
                           (sample [0.2] :size 1)))))
                     
           H   [(nrow ((first data-keys) data-hash))] 
           M   [(count initial-exemplars)]
           N   [(count initial-exemplars)]
           A   [(nn-self (matrix initial-exemplars) cliff-data)]           
           IPR [(iprmult cliff-data (matrix initial-exemplars) :query-size query-size :sav sav :worst worst)]]
      (println (str "lace " i " done"))      
      (if (or (empty? dat) (= (count dat) 1))
        [(save (matrix (first X))
               (str incanter-home "/save/clm11/cache/cache-"(.substring (str model-key) 1)"-"run".csv")
               :delim \, 
               :header ck-header)        
         (save (bind-columns (range 1 (inc (inc (count H))))
                             (conj H (+ (last H) (nrow ((last data-keys) data-hash))))
                             (conj M (count (first X)))
                             (conj N (count (second X)))
                             (conj A (nn-self (second X) ((first dat) data-hash)))
                             (conj IPR (iprmult (train0 ((first dat) data-hash)) (matrix (second X)) :query-size query-size :sav sav :worst worst)))
               (str incanter-home "/save/clm11/cmu/cmu-"(.substring (str model-key) 1)"-"run".csv")
               :delim \, 
               :header ["id" "#instances" "#exemplars" "data-ex" "acc" "ipr"])
         (println "lace done")]
        (recur
          (inc i)
          (rest dat)
          
          (loop [mm (sample [0.15 0.2 0.25 0.3 0.35] :size 1)
                 cq (sample [0.2] :size 1)]
            
            (let [ans (follow-the-leader 
                        (first X)
                        (cliff-fnc (train0 ((second dat) data-hash)) :m mm :q cq :sav sav)
                        dist-s mm :sav sav)]
              (if (crit-rest (train0 ((second dat) data-hash))
                             (matrix (second ans))
                             ip ac sav)
                ans
                (recur
                  (sample [0.15 0.2 0.25 0.3 0.35] :size 1)
                  (sample [0.2] :size 1))))) 
          (conj H (+ (last H) (nrow ((first dat) data-hash))))
          (conj M (count (first X)))
          (conj N (count (second X)))
          (conj A (nn-self (matrix (second X)) (train0 ((first dat) data-hash))))
          (conj IPR (iprmult (train0 ((first dat) data-hash)) (matrix (second X)) :query-size query-size :sav sav :worst worst))
          )))))