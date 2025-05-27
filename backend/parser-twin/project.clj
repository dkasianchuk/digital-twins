(defproject parser-twin "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [org.clojure/core.async "1.8.741"]
                 [org.antlr/antlr4-runtime "4.9.3"]
                 [org.antlr/antlr4 "4.9.3"]
                 [me.raynes/fs "1.4.6"]
                 [metosin/jsonista "0.3.9"]
                 [ring/ring-core "1.14.1"]
                 [ring/ring-jetty-adapter "1.14.1"]
                 [metosin/reitit "0.8.0"]]
  :main ^:skip-aot parser-twin.api
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :source-paths ["src"])
