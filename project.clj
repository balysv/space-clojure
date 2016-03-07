(defproject space "0.1.0"
  :description "A minimal Newtonian gravity and collision simulator"
  :url "http://www.github.com/balysv/space"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/algo.generic "0.1.2"]
                 [quil "2.3.0"]]
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main ^:skip-aot space.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})