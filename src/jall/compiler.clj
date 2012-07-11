(ns jall.compiler
  (:require [jall.parser :as p]
            [jall.util :as u]
            [jall.generate :as gen]
            [jall.io :as jall-io]
            [fs.core :as fs]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]))

(defn produce-java-support-files
  "Clojure has to implement a Java interface in order for its interop functions to agree with Java generics. Generate these Java interface files as needed for the given `source-file`"
  [source-file parse-tree langs]
  (let [package (p/java-package parse-tree)
        class-name (u/class-name-from-file source-file)
        full-class-name (str package "." class-name)
        methods (p/blocks-as-methods (p/blocks parse-tree))]
    (gen/output-java-support-files full-class-name methods)))

(defn produce-ajvm-files
  "Return `File` records for all AJVM files that need to be produced for the given `source-file` of JAll code"
  [source-file parse-tree]
  (let [package (p/java-package parse-tree)
        class-name (u/class-name-from-file source-file)
        full-class-name (str package "." class-name)
        imports (p/blocks-as-imports (p/imports parse-tree))
        methods (p/blocks-as-methods (p/blocks parse-tree))]
    (gen/output-ajvm-files full-class-name imports methods)))

(defn reproduce-java-file
  "Transform JAll snippets in original source, returning a `File` record representing the new Java source code transformed"
  [source-file parse-tree langs]
  (let [package (p/java-package parse-tree)
        class-name (u/class-name-from-file source-file)
        full-class-name (str package "." class-name)]
    (gen/output-java-file full-class-name langs source-file)))

(defn find-source-files
  "Given a directory of source code, return all files that have the `.jall` extension"
  [source-dir]
  (filter (fn [f] (.endsWith (.getName f) ".jall"))
          (file-seq (io/file source-dir))))

(defn jall-parse-tree
  "Given a source file, return the given Parsley parse tree"
  [src-filename]
  (let [common-tree   (p/strict-parse :common src-filename)
        clj-tree      (p/strict-parse :clj src-filename)
        rb-tree       (p/strict-parse :rb src-filename)]
    (p/combine-parse-trees common-tree clj-tree rb-tree)))

(defn print-jall-output
  "Simply print the output of JAll compilation to `*out*`"
  [java-support-files ajvm-files java-file]
  (log/info "\n ----> Java Support Interfaces <----\n")
  (doseq [file java-support-files]
    (print (:content file)))
  (log/info "\n ----> AJVM Files <----\n")
  (doseq [file ajvm-files]
    (print (:content file)))
  (log/info "\n ----> Java File <----\n")
  (print (:content java-file)))

(defn write-jall-output
  "Write output of JAll compilation to appropriate paths beneath the given `project-root`"
  [project-root java-support-files ajvm-files java-file]
  (let [working-pom "resources/jall_pom.xml"]
    (log/debug "----> Writing Java Support Interfaces <----")
    (doseq [file java-support-files]
      (jall-io/prepare-and-write-file project-root file))
    (log/debug "----> Writing AJVM Files <----")
    (doseq [file ajvm-files]
      (jall-io/prepare-and-write-file project-root file))
    (log/debug "----> Writing Final Java File <----")
    (jall-io/prepare-and-write-file project-root java-file)
    (log/debug "----> Writing Working POM File <----")
    (fs/copy working-pom (clojure.string/join "/" [project-root "pom.xml"]))))

(defn compile-file
  "Given the name of a file, compile it with JAll.

   Return a vector of the three things generated from JAll compilation: Java support files (interfaces), AJVM auto-generated source, and the original Java (JAll) file with JAll snippets transformed."
  [src-filename]
  (let [parse-tree    (jall-parse-tree src-filename)
        ajvm-files    (produce-ajvm-files src-filename parse-tree)
        java-support-files (produce-java-support-files src-filename
                                                       parse-tree
                                                       (map (fn [file-record]
                                                              (:lang file-record)) ajvm-files))
        java-file     (reproduce-java-file src-filename
                                           parse-tree
                                           (map (fn [file-record]
                                                  (:lang file-record)) ajvm-files))]
    [java-support-files ajvm-files java-file]))

(defn emit
  "Expected to receive the output of `compile-file`, either print or write to the filesystem the output of a JAll compilation sequence."
  ([root-dir java-support-files ajvm-files java-file] (emit root-dir java-support-files ajvm-files java-file false))
  ([root-dir java-support-files ajvm-files java-file dry-run?]
     (if dry-run?
       (print-jall-output java-support-files
                          ajvm-files
                          java-file)
       (write-jall-output root-dir
                          java-support-files
                          ajvm-files
                          java-file))))

(defn process-src-file
  ([root-dir source-filename] (process-src-file root-dir source-filename false))
  ([root-dir source-filename dry-run?]
     (let [[java-support-files ajvm-files java-file] (compile-file source-filename)]
       (emit root-dir java-support-files ajvm-files java-file dry-run?))))

(defn process-src-dir
  ([root-dir source-dir] (process-src-dir root-dir source-dir false))
  ([root-dir source-dir dry-run?]
     (let [sources (find-source-files source-dir)]
       (doseq [src sources]
         (let [[java-support-files ajvm-files java-file] (compile-file src)]
           (emit root-dir java-support-files ajvm-files java-file dry-run?))))))