(defproject sicp-book-exercises "0.1.0-SNAPSHOT"
  :description "Project where I store my exercises solutions of SICP book"
  :author "Erveftick"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [metasoarous/oz "2.0.0-alpha5"]]
  :main ^:skip-aot sicp-book-exercises.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
