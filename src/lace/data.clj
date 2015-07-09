(ns lace.data
  (:use (incanter core stats io)))
  
(def incanter-home (System/getProperty "incanter.home")) 
(def ck-header [":wmc" ":dit" ":noc" ":cbo" ":rfc" ":lcom" ":ca" ":ce" ":npm" ":lcom3" ":loc" ":dam" 
                ":moa" ":mfa" ":cam" ":ic" ":cbm" ":amc" ":max_cc" ":avg_cc" ":bug"])

;----------------------------
; Z
;----------------------------

(def ant17z (read-dataset 
                 (str incanter-home "resources/data/ant17z-test.csv")
                 :delim \,
                 :header true))
(def ant17z-header (apply vector (map str (:column-names ant17z))))
(def ant17z (to-matrix ant17z))

(def camel16z (read-dataset 
                 (str incanter-home "resources/data/camel16z-test.csv")
                 :delim \,
                 :header true))
(def camel16z-header (apply vector (map str (:column-names camel16z))))
(def camel16z (to-matrix camel16z))

(def ivy20z (read-dataset 
                 (str incanter-home "resources/data/ivy20z-test.csv")
                 :delim \,
                 :header true))
(def ivy20z-header (apply vector (map str (:column-names ivy20z))))
(def ivy20z (to-matrix ivy20z))

(def jedit41z (read-dataset 
                 (str incanter-home "resources/data/jEdit41z-test.csv")
                 :delim \,
                 :header true))
(def jedit41z-header (apply vector (map str (:column-names jedit41z))))
(def jedit41z (to-matrix jedit41z))

(def lucene24z (read-dataset 
                 (str incanter-home "resources/data/lucene24z-test.csv")
                 :delim \,
                 :header true))
(def lucene24z-header (apply vector (map str (:column-names lucene24z))))
(def lucene24z (to-matrix lucene24z))

(def poi30z (read-dataset 
                 (str incanter-home "resources/data/poi30z-test.csv")
                 :delim \,
                 :header true))
(def poi30z-header (apply vector (map str (:column-names poi30z))))
(def poi30z (to-matrix poi30z))

(def synapse12z (read-dataset 
                 (str incanter-home "resources/data/synapse12z-test.csv")
                 :delim \,
                 :header true))
(def synapse12z-header (apply vector (map str (:column-names synapse12z))))
(def synapse12z (to-matrix synapse12z))

(def velocity161z (read-dataset 
                 (str incanter-home "resources/data/velocity161z-test.csv")
                 :delim \,
                 :header true))
(def velocity161z-header (apply vector (map str (:column-names velocity161z))))
(def velocity161z (to-matrix velocity161z))

(def xalan26z (read-dataset 
                 (str incanter-home "resources/data/xalan26z-test.csv")
                 :delim \,
                 :header true))
(def xalan26z-header (apply vector (map str (:column-names xalan26z))))
(def xalan26z (to-matrix xalan26z))

(def xerces13z (read-dataset 
                 (str incanter-home "resources/data/xerces13z-test.csv")
                 :delim \,
                 :header true))
(def xerces13z-header (apply vector (map str (:column-names xerces13z))))
(def xerces13z (to-matrix xerces13z))

(def p1v192 (read-dataset 
                 (str incanter-home "resources/data/p1v192-originals.csv")
                 :delim \,
                 :header true))
(def p1v192-header (apply vector (map str (:column-names p1v192))))
(def p1v192 (to-matrix p1v192))

(def p2v276 (read-dataset 
                 (str incanter-home "resources/data/p2v276-originals.csv")
                 :delim \,
                 :header true))
(def p2v276-header (apply vector (map str (:column-names p2v276))))
(def p2v276 (to-matrix p2v276))

(def p3v318 (read-dataset 
                 (str incanter-home "resources/data/p3v318-originals.csv")
                 :delim \,
                 :header true))
(def p3v318-header (apply vector (map str (:column-names p3v318))))
(def p3v318 (to-matrix p3v318))

(def p4v362 (read-dataset 
                 (str incanter-home "resources/data/p4v362-originals.csv")
                 :delim \,
                 :header true))
(def p4v362-header (apply vector (map str (:column-names p4v362))))
(def p4v362 (to-matrix p4v362))

(def p42v454 (read-dataset 
                 (str incanter-home "resources/data/p42v454-originals.csv")
                 :delim \,
                 :header true))
(def p42v454-header (apply vector (map str (:column-names p42v454))))
(def p42v454 (to-matrix p42v454))

(def p43v512 (read-dataset 
                 (str incanter-home "resources/data/p43v512-originals.csv")
                 :delim \,
                 :header true))
(def p43v512-header (apply vector (map str (:column-names p43v512))))
(def p43v512 (to-matrix p43v512))

(def p5v185 (read-dataset 
                 (str incanter-home "resources/data/p5v185-originals.csv")
                 :delim \,
                 :header true))
(def p5v185-header (apply vector (map str (:column-names p5v185))))
(def p5v185 (to-matrix p5v185))