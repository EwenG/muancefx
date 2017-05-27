(defproject muance/muancefx "0.0.1"
  :source-paths ["src"]
  :java-source-paths ["java"]
  :test-paths ["test"]
  :resource-paths ["resources"]
  :dependencies []
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.9.0-alpha15"]
                                  [org.clojure/clojurescript "1.9.521"]
                                  [muance/muance "0.0.1"]]}}
  :plugins [[lein-cljsbuild "1.1.5"]]
  :cljsbuild {:builds
              [{:source-paths ["src"]
                :compiler {:output-to "resources/app.min.js"
                           :optimizations :advanced
                           :externs ["externs/muancefx-externs.js"]}}]}
  :aot []
  :main muancefx.Main)
