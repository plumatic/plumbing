(defproject prismatic/plumbing "0.2.1-SNAPSHOT"
  :description "Prismatic's Clojure utility belt."
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}
  :url "https://github.com/Prismatic/plumbing"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [prismatic/schema "0.2.0"]
                 [de.kotka/lazymap "3.1.0" :exclusions [org.clojure/clojure]]]
  :profiles {:1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}
             :dev {}}
  :jvm-opts ^:replace []
  :aliases {"all" ["with-profile" "dev:dev,1.4"]})
