(ns jall.lang.Compiler
  (:use [clojure.tools.cli :only [cli]])
  (:require [jall.compiler :as compiler])
  (:gen-class
   :methods [^{:static true} [processSrcFile [String String] void]
             ^{:static true} [processSrcDir  [String String] void]]))

(defn -processSrcFile
  "Java-facing method for compiling a single JAll file"
  [root-dir source-filename]
  (compiler/process-src-file root-dir source-filename))

(defn -processSrcDir
  "Java-facing method for compiling all JAll source files in a given `source-dir`"
  [root-dir source-dir]
  (compiler/process-src-dir root-dir source-dir))

(defn -main
  "Process JAll source files"
  [& args]
  (let [[options args banner] (cli args
                                   ["-s" "--source-dir" "Source directory" :default "src/main/jall"]
                                   ["-t" "--root-dir" "Root of project" :default "."]
                                   ["-p" "--support-dir" "Destination for auto-generated Java support files" :default "target/src/support/java"]
                                   ["-c" "--clojure-dir" "Destination for auto-generated Clojure files" :default "target/src/main/clojure"]
                                   ["-r" "--ruby-dir" "Destination for auto-generated Ruby files" :default "target/src/main/ruby"]
                                   ["-l" "--scala-dir" "Destination for auto-generated Scala files" :default "target/src/main/scala"]
                                   ["-d" "--dry-run" "Parse files and print what would be written to disk to STDOUT" :default false :flag true]
                                   ["-h" "--help" "Show help" :default false :flag true])]
    (when (:help options)
      (println banner)
      (System/exit 0))
    (compiler/process-src-dir (:root-dir options) (:source-dir options) (:dry-run options))))
