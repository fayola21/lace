(ns lace.ipr
  (:use (incanter core stats))
  (:use (clojure.contrib combinatorics))
  (:use (lace utils)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Query Generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-queries-one
  "
  Returns queries from one instance, making sure that any sensitive attributes
  and the class values are not included in the query.

  Arguments:
    one -- instance to generate queries from
    n -- length of a query
    sav -- (default [10 20]) 
  
  References:
    http://spare.lero.ie/pdf/lace2.pdf 

  "
  [one n sav]
  (let [atts   (range (count one))
        qids   (remove (set sav) atts)
        combos (combinations qids n)
        query  (fn [one combo]
                 (apply vector (map #(vector % (nth one %)) combo)))]
    (apply vector (map #(query one %) combos))))

    
(defn get-queries 
  "
  
  Returns queries generated from 100 randomly chosen instances. Apply 
  get-queries-one to each instance, then apply concat and distinct to remove 
  duplicate queries.

  Arguments:
    bin-data -- original data is binned using equal frequency binning
    n -- length of a query
    sav -- (default [10 20]) 

  "
  [bin-data n sav & {:keys [num]
                     :or {num 100}}]
  (distinct (apply concat (map #(get-queries-one % n sav) (take num (shuffle (to-vect bin-data)))))))

        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Attacker's Best Guess
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn best-guess
  "
  
  Returns multiple sensitive attribute values based on instances resulting from
  a query.

  Arguments:
    data-q -- instances resulting from a query
    atts -- the column numbers of the sav being tested.
  
  References:
    http://spare.lero.ie/pdf/lace2.pdf 

  "
  [data-q atts]
  (loop [att atts result []]
    (if (empty? att)
      result
      (recur
        (rest att)
        (conj result 
              (cond
                (empty? data-q)     1111111
                (= (nrow data-q) 1) (nth (first data-q) (first att))
                (apply = (nth (trans data-q) (first att))) (nth (first data-q) (first att)) ;(first (first (sort-by last > (frequencies (sel data :cols (first att))))))
                :else               (first (first (sort-by last > (frequencies (nth (trans data-q) (first att))))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Query score or IPR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn best-guesses 
  "
  
  Returns multiple sensitive attribute values based on instances resulting from
  queries. Or 'best guess' values of attacker for each sensitive attribute in 
  sav.

  Arguments:
    query -- models attacker's background knowledge, it can be a vector of attribute value pairs
    mybin -- original data binned with equal frequency binning
    data -- original data
    atts -- the column numbers of the sav being tested.
  
  References:
    http://spare.lero.ie/pdf/lace2.pdf 

  "
  [query mybin data atts]
  (let [bdata   (map #(apply vector (sort %)) (to-vect (trans mybin)))
        get-bin (fn [attnum query-val]
                  (let [att-col (distinct (nth bdata attnum))
                        x (val-idx query-val att-col)
                        prev-query (if (= x 0) 0 (nth att-col (dec x)))]
                    prev-query))]
    (loop [one-att query d data]
      (if (or (empty? one-att) (empty? d) (= (nrow d) 1))
        (best-guess d atts) 	
        (recur
          (rest one-att)
          (let [maxi   (filter #(<= (nth % (first (first one-att))) (second (first one-att))) d)
                minbin (get-bin (first (first one-att)) (second (first one-att)))]
            (if (or (= minbin 0) (<= (nrow maxi) 1))
              maxi
              (filter #(> (nth % (first (first one-att))) minbin) (matrix maxi)))))))))


(defn more-best-guesses
  "
  
  Returns a matrix showing an attacker's 'best-guess' for sensitive attributes
  after applying multiple queries. 

  Arguments:
    queries1 -- models attacker's background knowledge, it can be a vector of vectors of attribute value pairs
    mybin -- original data binned with equal frequency binning
    data -- original data
    atts -- the column numbers of the sav being tested
    query size -- no. of attribute value pairs in a query
    beforafter -- indicator of data, original or private, 4 = before, 10 = after
  
  References:
    http://spare.lero.ie/pdf/lace2.pdf 

  "
  [queries1 mybin data qs12 atts query-size beforeafter]
  (let [ans (matrix (map #(qs12 % mybin data atts) queries1))
        x   (matrix (repeat (nrow ans) beforeafter))
        x1  (matrix (range 1 (inc (nrow ans))))
        y   (matrix (repeat (nrow ans) query-size))
        _   (assert (> (nrow ans) 1))]
    (bind-columns x1 x y (sel ans :cols (range 0 (ncol ans))))))


(defn get-acc [want-got n] ; util
  (let [want     (apply vector (take n want-got))
        got      (apply vector (drop n want-got))
        together (map #(vector %1 %2) want got)]
    (loop [t together result 0]
      (if (empty? t)       
        result
        (recur (rest t)                              
               (if (= (Math/ceil (first (first t))) (Math/ceil (second (first t))))		 
                 (inc result)
                 result))))))


(defn get-accs 
  "
  
  Returns IPRs. 

  Example:
    Used in function iprmult

  Arguments:
    wants-gots -- matrix with sensitive attribute values from original data and obfuscated version of data
    n -- number of queries
  
  References:
    http://spare.lero.ie/pdf/lace2.pdf 

  "
  [wants-gots n]
  (let [_     (assert (> n 1))
        total (* n (ncol wants-gots))
        wgs   (if (= (ncol wants-gots) 1) [wants-gots] (trans wants-gots))]
    (loop [want-got wgs result 0]
      (if (empty? want-got)
        (- 100 (* 100.0 (/ result total)))
        (recur
          (rest want-got)
          (+ result (get-acc (first want-got) n)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;IPR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn iprmult 
  "
  Accepts the original data and a privatized data candidate from the original 
  data. The IPR score is returned.

  Options:
  :sav (default [10 20])
  :n (default 10), number of bins for efb2
  :query-size (default 1)
  :num (default 1000), 1000 queries generated at most

  References:
  http://spare.lero.ie/pdf/lace2.pdf 
  
  "
  [original-data private-data & {:keys [sav n query-size num]
                                 :or {sav [10 20] n 10 query-size 1 num 1000}}]  
  (let [mybin   (bind-columns (trans (butlast (efb2 original-data n))) (sel original-data :cols (dec (ncol original-data))))
        que     (take num (shuffle (get-queries mybin query-size sav :num num)))
        ans     (bind-rows
                  (more-best-guesses que mybin original-data best-guesses (butlast sav) query-size 4)
                  (more-best-guesses que mybin private-data best-guesses (butlast sav) query-size 10))
        scores  (get-accs (sel ans :cols (range 3 (ncol ans))) (count que))]
    scores))