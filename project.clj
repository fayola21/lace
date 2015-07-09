(defproject lace "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojure-contrib "1.2.0"]		             
                 [incanter/incanter-core "1.4.1"]
                 [incanter/incanter-io "1.4.1"]
                 [org.clojure/tools.cli "0.3.1"]]
  :aot [lace.core]
  :main lace.core)
