(defproject prismatic/plumbing "0.5.6-SNAPSHOT"
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
                   :plugins [[com.keminglabs/cljx "0.6.0" :exclusions [org.clojure/clojure]]
                             [codox "0.10.8"]
                             [lein-cljsbuild "1.1.8"]
                             [lein-doo "0.1.10"]]
                   :cljx {:builds [{:source-paths ["src"]
                                    :output-path "target/generated/src/clj"
                                    :rules :clj}
                                   {:source-paths ["src"]
                                    :output-path "target/generated/src/cljs"
                                    :rules :cljs}
                                   {:source-paths ["test"]
                                    :output-path "target/generated/test/clj"
                                    :rules :clj}
                                   {:source-paths ["test"]
                                    :output-path "target/generated/test/cljs"
                                    :rules :cljs}]}}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :1.9 {:dependencies [[org.clojure/clojure "1.9.0"]]}
             :1.11 {:dependencies [[org.clojure/clojure "1.11.0-master-SNAPSHOT"]]
                    :repositories [["sonatype-oss-public" {:url "https://oss.sonatype.org/content/groups/public"}]]}}

  :jar-exclusions [#"\.cljx"]
  :aliases {"all" ["with-profile" "+1.8:+1.9:+dev:+1.11"]
            "deploy" ["do" "clean," "cljx" "once," "deploy" "clojars"]
            "test" ["do" "clean," "cljx" "once," "test," "doo" "node" "test" "once"]}

  :lein-release {:deploy-via :shell
                 :shell ["lein" "deploy"]}

  :auto-clean false

  :source-paths ["target/generated/src/clj" "src"]

  :resource-paths ["target/generated/src/cljs"]

  :test-paths ["target/generated/test/clj" "test"]

  :cljsbuild {:builds
              {:dev {:source-paths ["src"
                                    "target/generated/src/clj"
                                    "target/generated/src/cljs"]
                     :compiler {:output-to "target/main.js"
                                :optimizations :whitespace
                                :pretty-print true}}
               :test {:source-paths ["src"
                                     "test" ;; for plumbing.test-runner
                                     "target/generated/src/clj"
                                     "target/generated/src/cljs"
                                     "target/generated/test/clj"
                                     "target/generated/test/cljs"]
                      :compiler {:output-to "target/unit-test.js"
                                 :main plumbing.test-runner
                                 :target :nodejs
                                 :pretty-print true}}}}

  :codox {:src-uri-mapping {#"target/generated/src/clj" #(str "src/" % "x")}
          :src-dir-uri "http://github.com/plumatic/plumbing/blob/master/"
          :src-linenum-anchor-prefix "L"}

  :jvm-opts ^:replace [])
