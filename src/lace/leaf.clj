(ns lace.leaf
  (:use (incanter core stats))
  (:use (lace ipr morph utils)))


(defn follow-the-leader
  "
 
  Returns updated exemplar data. Each instance of each new data is checked to 
  see if it's a leader or follower. Followers are discarded and leaders are
  kept.
  
  Arguments:
    X -- initial exemplar data, starts the cache
    data -- new data set
    dist-s -- leaf-multi
    m -- random value to determine boundary in morph-one
  
  References:
    http://spare.lero.ie/pdf/lace2.pdf
  
  "

  [X data dist-s m & {:keys [sav distance-fnc]
                      :or {sav [10 20] distance-fnc euclidean-distance}}]
  (loop [dat data exemplars0 X new-xm [] orig []]
    (if (empty? dat)
      (let [cache (remove #(= nil %) exemplars0)
            new-exemplars (drop (count X) (reverse cache))
            xms   (remove #(= nil %) new-xm)]
        [cache new-exemplars (remove #(= nil %) orig)])
      (recur
        (rest dat)
        (conj 
          exemplars0 
          (let [ans     (get-nearest1 (first dat) (remove #(= nil %) exemplars0) distance-fnc)
                pt      (first ans) ; Nearest exemplar from result.
                dist-pt (second ans)
                label   (last pt)]
            (if (< dist-pt dist-s)
              nil              
              (trans (morph-one (remove #(= nil %) exemplars0) (first dat) sav m distance-fnc)))))
        (conj 
          new-xm
          (let [ans     (get-nearest1 (first dat) (remove #(= nil %) exemplars0) distance-fnc)
                pt      (first ans) ; Nearest exemplar from result.
                dist-pt (second ans)
                label   (last pt)]
            (if (< dist-pt dist-s)
              nil              
              (trans (morph-one (remove #(= nil %) exemplars0) (first dat) sav m distance-fnc)))))
        (conj 
          orig
          (let [ans     (get-nearest1 (first dat) (remove #(= nil %) exemplars0) distance-fnc)
                pt      (first ans) ; Nearest exemplar from result.
                dist-pt (second ans)
                label   (last pt)]
            (if (< dist-pt dist-s)
              nil              
              (first dat))))))))


(defn leaf-one
  "Morphing a single instance, given exemplars and one instance."
  [data one distance-fnc]
  (let [grouped   (group-by last data)
        defects   (get grouped 1.0)
        nodefects (get grouped 0.0)
        onelabel  (last one)
        nun-d     (if (= onelabel 0.0)
                    (second (get-nearest1
                             one
                             (if (= (nrow defects) 1) [defects] defects)
                             distance-fnc))
                    (second (get-nearest1
                             one
                             (if (= (nrow nodefects) 1) [nodefects] nodefects)
                             distance-fnc)))]
    nun-d))


(defn leaf-multi 
  "
  Returns the dist-s used in leaf1 and leaf2 to determine leaders and followers.
  "
  [data distance-fnc num p]
  (let [dat (take num (shuffle (to-vect data)))]
    (loop [d dat result []]
      (if (empty? d)
        (* p (median result))
        (recur
          (rest d)
          (conj result (leaf-one data (first d) distance-fnc)))))))


(defn leaf1
  "
  Returns a list of exemplars from initiator data.
	
    p is decimal input for distance boundary, 0.1.
    only-exemplars: return only the exemplars otherwise return exemplars vectored with dist-s.

  References:
    http://spare.lero.ie/pdf/lace2.pdf
  "
  [data & {:keys [p only-exemplars num distance-fnc]
           :or {p 0.1 only-exemplars true num 100 distance-fnc euclidean-distance}}]
  (let [dist-s  (leaf-multi data distance-fnc num p)]
    (loop [dat data result [(first data) (last data)]]
      (if (empty? dat)
        (if only-exemplars
          (matrix (remove #(= nil %) result))
          [(remove #(= nil %) result) dist-s])
        (recur
          (rest dat)
          (conj result (let [ans     (get-nearest1 (first dat) (remove #(= nil %) result) distance-fnc)
                             pt      (first ans) ; Nearest exemplar from result.
                             dist-pt (second ans)
                             label   (last pt)]
                         (if (< dist-pt dist-s)
                           nil
                           (first dat)))))))))


(defn leaf2
  "
  Returns a list of exemplars from data other than initiator.

	  p is decimal input for distance boundary, 0.1.
    only-exemplars: return only the exemplars otherwise return exemplars vectored with dist-s.

  References:
    http://spare.lero.ie/pdf/lace2.pdf
  "
  [data dist-s & {:keys [p only-exemplars num distance-fnc]
                  :or {p 0.1 only-exemplars true num 100 distance-fnc euclidean-distance}}]
    (loop [dat data result [(first data) (last data)]]
      (if (empty? dat)
        (if only-exemplars
          (matrix (remove #(= nil %) result))
          [(remove #(= nil %) result) dist-s])
        (recur
          (rest dat)
          (conj result (let [ans     (get-nearest1 (first dat) (remove #(= nil %) result) distance-fnc)
                             pt      (first ans) ; Nearest exemplar from result.
                             dist-pt (second ans)
                             label   (last pt)]
                         ;(if (and (= (last (first dat)) label) (< dist-pt dist-s))
                         (if (< dist-pt dist-s)
                           nil
                           (first dat))))))))