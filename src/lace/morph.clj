(ns lace.morph
  (:use (incanter core stats))
  (:use (lace utils)))

(declare new-x jiggle5)

(defn morph [data & {:keys [sav m distance-fnc]
                     :or {sav [10 20] m 0.5 distance-fnc euclidean-distance}}]
  "
 
  Returns a morphed version of data.
  
  Arguments:
    data -- data set to be morphed
  
  Options:
    :sav          (default [10 20]) , columns specific to defect data sets, 10 is lines of code and 20 is the class for each instance.
    :m            (default 0.5), determines the boundary between the instances with unlike classes.
    :distance-fnc (default euclidean-distance)   
  
  References:
    http://spare.lero.ie/pdf/lace2.pdf
  
  "
  (jiggle5 data sav m distance-fnc))


(defn morph-one
  "Morphing a single instance, given exemplars and one instance."
  [data one sav m distance-fnc]
  (let [grouped   (group-by last data)
        defects   (get grouped 1.0)
        nodefects (get grouped 0.0)
        onelabel  (last one)
        nun-x     (if (= onelabel 0.0)
                    (first (get-nearest1
                             one
                             (if (= (nrow defects) 1) [defects] defects)
                             distance-fnc))
                    (first (get-nearest1
                             one
                             (if (= (nrow nodefects) 1) [nodefects] nodefects)
                             distance-fnc)))]
    (new-x data [one one] sav m nun-x)))


(defn new-x [X x sav m near-x]
  "
 
  Returns a morphed version of an instance (x) of X. Used by jiggle5.

  Arguments:
    X -- data set to be morphed
    x -- instance of X to be morphed
    sav -- (default [10 20])
    m -- (default 0.5)
    near-x -- nearest unlike neighbor of x   
  
  "
  (loop [x1 0 result []]
    (if (= (count (first x)) x1)                                   
      result                                        
      (recur 
        (inc x1)
        (conj result 
              (if (member? x1 sav) 
                (nth (first x) x1) 
                (if (= (rand-int 2) 0)
                  (let [big (+ (nth (first x) x1) 
                               (* (abs (- (nth (first x) x1) (nth near-x x1))) m))
                        big1 (if (> big (apply max (nth (to-vect (trans X)) x1)))
                               (apply max (nth (to-vect (trans X)) x1))
                               big)]
                    big)
                  (let [small (- (nth (first x) x1) 
                                 (* (abs (- (nth (first x) x1) (nth near-x x1))) m))
                        small1 (if (< small (apply min (nth (to-vect (trans X)) x1)))
                                 (apply min (nth (to-vect (trans X)) x1))
                                 small)]
                    small1))))))))


(defn jiggle5 [X sav m distance-fnc]
  "
 
  Returns a morphed version of data (X). For each instance in X, find nearest 
  unlike neighbor then morph the instance without crossing boundary using new-x.
  
  Arguments:
    X -- data set to be morphed
    sav -- (default [10 20])
    m -- (default 0.5)
    distance-fnc -- (default euclidean-distance)   
  
  "
  (let [separate-X (sort-by first (group-by last X))
        buggy (matrix (second (second separate-X)))
        clean (matrix (second (first separate-X)))]
    (loop [x X result1 []]
      (if (empty? x)
        (matrix result1)
        (recur
          (rest x)
          (conj 
            result1 
            (let [near-x (if (= (last (first x)) (if (= (nrow clean) 1) (last clean) (last (first clean)))) ;;find nun
                           (first 
                             (get-nearest1 
                               (first x) 
                               (if (= (nrow buggy) 1) [buggy] buggy) 
                               distance-fnc))
                           (first 
                             (get-nearest1 
                               (first x) 
                               (if (= (nrow clean) 1) [clean] clean) 
                               distance-fnc)))	
                  newx (new-x X x sav m near-x)]		
              newx)))))))
