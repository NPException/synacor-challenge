(defproject synacor-challenge "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.11.0-alpha3"]
                 [criterium "0.4.6"]]
  ;; :main ^:skip-aot synacor-challenge.core
  :target-path "target/%s"
  :repl-options {:init-ns synacor-challenge.core}
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
