(ns lace.utils
  (:use (incanter core stats io)))

(defn compress [x]
  (let [ans (frequencies x)
        one (map first ans)
        two (map second ans)]
    (map #(list %1 %2) two one)))

(defn member? [val lst]
  (get (set lst) val))

(defn val-idx [val lst]
  (get (zipmap lst (range (count lst))) val))

(defn get-nearest1 [one data distance-fnc]
  "Returns vector of nearest instance and distance. All input has class label."
  (first (sort-by second (map #(vector % (distance-fnc (butlast one) (butlast %))) data))))

(defn get-farthest1 [one data distance-fnc]
  "Returns vector of farthest instance and distance. All input has class label."
  (first (sort-by second > (map #(vector % (abs (distance-fnc (butlast one) (butlast %)))) data)))) 


(defn change-to-matrix
  "Accept a filename (string with pathname) and output a matrix from the data."
  [filename]
  (to-matrix (read-dataset filename :delim \, :header true)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EQUAL FREQUENCY BINNING FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
(defn sane-numbers [data]
"Ensures that no noise is used in equal frequency or width binning"
  (sort (filter #(number? %) data)))

(def OUT [])
(def N 1)

(defn breaks1 [x l b4 max-size]
  (loop [x x lx (rest l) b4 nil]
    (when-not (= x b4)
      (when (>= N max-size)
        (def N 0)
        (def OUT (conj OUT x))))
    (cond 
      (not-empty lx)
      (and (def N (inc N))
           (recur (first lx) (rest lx) x))
      true (if (not (get (set OUT) x)) ;;(not (member? x OUT))
             (def OUT (conj OUT x))))))

(defn efb1 [x break-at]
  (if (= x '?)
    x
    (loop [bk break-at]
      (if (<= x (first bk))
        (first bk)
        (recur (rest bk))))))

(defn breaks [l max-size]
  (breaks1 (first l) (rest l) 'nil  max-size)
  (let [result OUT]
    (def OUT []) ;reset
    (def N 1) ;reset
    result))

(defn efb
  ([data] (efb data 10))
  ([data nb] ;nb = no. of bins
    (let [numbers  (sane-numbers data)
          want     (Math/round (/ (count numbers) (* nb 1.0)))
          ba       (breaks numbers want)
          break-at (concat (butlast ba) [(last numbers)])
          ;;break-at (concat (butlast ba) [1000000])  ;(breaks numbers want)
          new      (map (fn [datum] (efb1 datum break-at))  data)]
      new)))

(defn efb2
  ([data] (efb2 data 10))
  ([data n]
    (let [dat (to-vect (trans data))]
      (loop [d dat result []]
        (if (empty? d)
          result
          (recur (rest d) 
                 (conj result (efb (first d) n))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BORE FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;			

(defn bin-rank [D val col best rest]
  "D = binned data set"
  (let [pbest (/ (nrow best) (nrow D))
        prest (/ (nrow rest) (nrow D))        
        freqEbest (fn []
                    (let [one (filter #(= (nth % col) val) best)] 
                      (if (= (nrow one) 0)
                        0
                        (let [freqEbest0 (count
                                           (filter #(=
                                                      (nth % col)
                                                      val) best)) 
                              freqEbest1 (/ freqEbest0 (nrow best))]
                          freqEbest1))))        
        freqErest (fn []
                    (let [two (filter #(= (nth % col) val) rest)]
                      (if (= (nrow two) 0)
                        0
                        (let [freqErest0 (count
                                           (filter #(=
                                                      (nth % col)
                                                      val) rest))
                              freqErest1 (/ freqErest0 (nrow rest))]
                          freqErest1))))        
        likebestE (* (freqEbest) pbest)
        likerestE (* (freqErest) prest)
        rank (/ (Math/pow likebestE 2) (+ likebestE likerestE))]
    [val rank col]))

(defn get-ranks-only [D best rest]
  (let [get-ranks1 (fn [col]
                     (let [vals (map first (frequencies (sel D :cols col)))] ;;(uc (to-vect (sel D :cols col)))]
                       (map #(bin-rank D % col best rest) vals)))
        get-ranks2 (map #(get-ranks1 %) (range 0 (- (ncol D) 1)))]
    get-ranks2))

(defn bore
  "BORE with output [val rank attribute]"
  [D best rest]
  (get-ranks-only D best rest))