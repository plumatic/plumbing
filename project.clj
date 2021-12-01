(defproject prismatic/plumbing "0.6.0-SNAPSHOT"
  :description "Prismatic's Clojure utility belt."
  :url "https://github.com/plumatic/plumbing"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}

  :dependencies [[prismatic/schema "1.2.0"]
                 [de.kotka/lazymap "3.1.0" :exclusions [org.clojure/clojure]]]

  :profiles {:dev {:dependencies [[org.clojure/clojure "1.10.3"]
                                  [org.clojure/clojurescript "1.10.891"]
                                  [org.clojure/core.async "1.4.627"]]
                   :plugins [[codox "0.10.8"]
                             [lein-cljsbuild "1.1.8"]
                             [lein-doo "0.1.10"]]}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :1.9 {:dependencies [[org.clojure/clojure "1.9.0"]]}
             :1.11 {:dependencies [[org.clojure/clojure "1.11.0-master-SNAPSHOT"]]
                    :repositories [["sonatype-oss-public" {:url "https://oss.sonatype.org/content/groups/public"}]]}}

  :aliases {"all" ["with-profile" "+1.8:+1.9:+dev:+1.11"]
            "deploy" ["do" "deploy" "clojars"]
            "test" ["do" "test," "doo" "node" "test" "once"]}

  :lein-release {:deploy-via :shell
                 :shell ["lein" "deploy"]}

  :source-paths ["src"]
  :test-paths ["test"]

  :cljsbuild {:builds
              {:dev {:source-paths ["src"]
                     :compiler {:output-to "target/main.js"
                                :optimizations :whitespace
                                :pretty-print true}}
               :test {:source-paths ["src" "test"]
                      :compiler {:output-to "target/unit-test.js"
                                 :main plumbing.test-runner
                                 :target :nodejs
                                 :pretty-print true}}}}

  :codox {:src-dir-uri "http://github.com/plumatic/plumbing/blob/master/"
          :src-linenum-anchor-prefix "L"}

  :jvm-opts ^:replace [])
