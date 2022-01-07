(defproject synacor-challenge "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.1"]]
  ;; :main ^:skip-aot synacor-challenge.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
