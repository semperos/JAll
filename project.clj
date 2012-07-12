(defproject jall "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [net.cgrand/parsley "0.9.1"]
                 [fs "1.2.0"]
                 [org.clojure/tools.cli "0.2.1"]
                 [org.clojure/tools.logging "0.2.4"]]
  :dev-dependencies [[expectations "1.4.4"]]
  :aot [jall.lang.Compiler]
  :main jall.lang.Compiler)