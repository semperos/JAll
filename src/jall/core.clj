;; ## JAll ##
;;
;; Write Clojure, JRuby or Scala in your Java code. If this proof of concept works,
;; then mix and match those options as you see fit.
;;
;; The concept is to provide a *functional templating language* that provides compile-time
;; constructs for writing in multiple JVM languages, without having to setup a proper polyglot build yourself.
;; This is targeted at the
;; developer who would like to continue to write Java primarily, but would also like to
;; take advantage of the expressivity of alternative JVM languages. Each JVM language
;; has a set of strengths: JRuby has access to elegant Ruby DSL's for things like XML
;; generation, Clojure's data structure manipulation functions are second to none,
;; and Scala integrates tightly with Java semantics while providing functional data
;; structures and pattern matching.
;;
;; What is a *functional templating language*? For JAll, this means that you can write
;; non-Java code in your Java file that will be translated to functions (in Java parlance,
;; the equivalent of static method calls). Under the hood, the templated snippets
;; will be extracted from the Java file at compile time and placed in separate files
;; to generate legitimate Java-consumable classes in the alternative JVM language.
;; For JRuby, this means writing JRuby classes and having them processed with the proper
;; `jrubyc` options to generate actual Java classes. For Clojure, this means using
;; `gen-class` to generate a real Java class at compile time.
;;
;; Once the snippets have been extracted and proper classes created with them,
;; JAll will replace the snippets in the original file with calls to the generated
;; classes.
;;
;; If a serious amount of code is desired in separate JVM languages, it makes more sense
;; to fully learn that language and create a proper polyglot build environment. JAll's
;; sweet spot is for those instances where you want to leverage alternative JVM languages
;; in random corners of your otherwise Java-based application.
;;
;; ### Code Examples ###
;;
;; Work in progress - how we think JAll should look.
;;
;; Snippets will actually be named functions. There will be no conventions for coercing
;; data types from Java to JRuby/Clojure/Scala types; the code written in the
;; alternative JVM language (AJVM) should accept *and return* a real Java value.
;; The developer is responsible for understanding how each language handles this.
;;
;; Named snippet functions can then be reused in the same file, or in another file
;; if properly name-spaced, by using the name given. Methods inherit the package
;; and class name of the Java file they are defined in as shown below.
;;
;;     package com.example;
;;
;;     public class Hello {
;;         private String name = "";
;;         private final Array<String> names = ["John", "Sally", "Susan", "Robert"]
;;
;;         public Hello(name) {
;;             this.name = name;
;;         }
;;
;;         public void sayHello() {
;;             System.out.println("Hello, " + this.name);
;;         }
;;
;;         public void sayHelloToEveryone() {
;;             !def clj_say-hello-to-everyone(arr-of-names) {{
;;                 (doseq [n arr-of-names]
;;                   (println (str "Hello, " n)))
;;             }}
;;             !clj_say-hello-to-everyone(names);
;;         }
;;
;;         public void sayHelloToEveryoneLoudly() {
;;             !def clj_say-hello-to-everyone-loudly(arr-of-names) {{
;;                 (let [names (map #(.toUpperCase %) arr-of-names)]
;;                   (doseq [n names]
;;                     (println (str "Hello, " n))))
;;             }}
;;             !clj_say-hello-to-everyone-loudly(names)
;;         }
;;     }
;;
;; Though the above Clojure snippets are not particularly "powerful" examples
;; of what JAll can facilitate, I think it demonstrates the opportunities
;; it creates. The above would be transformed at compile time into a Clojure file
;; that looks like this:
;;
;;     (ns com.example.HelloClj
;;       (:gen-class
;;        :name com.example.HelloClj
;;        :constructors [[]]
;;        :prefix "java-"
;;        :methods [^:static [say-hello-to-everyone [names] nil]
;;                  ^:static [say-hello-to-everyone-loudly [names] nil]]))
;;
;;     (defn java-say-hello-to-everyone
;;       [arr-of-names] ;; taken from snippet
;;       ;; code below copied directly from snippet with final parens added
;;       (doseq [n arr-of-names]
;;         (println (str "Hello, " n))))
;;
;;     (defn java-say-hello-to-everyone-loudly
;;       [arr-of-names] ;; taken from snippet
;;       ;; code below copied directly from snippet with final parens added
;;       (let [names (map #(.toUpperCase %) arr-of-names)]
;;         (doseq [n names]
;;           (println (str "Hello, " n)))))
;;
;; And the original Java code will be transformed to the following:
;;
;;     package com.example;
;;
;;     // ADDED
;;     import com.example.HelloClj;
;;
;;     public class Hello {
;;         private String name = "";
;;         private final Array<String> names = ["John", "Sally", "Susan", "Robert"]
;;
;;         public Hello(name) {
;;             this.name = name;
;;         }
;;
;;         public void sayHello() {
;;             System.out.println("Hello, " + this.name);
;;         }
;;
;;         public void sayHelloToEveryone() {
;;             // TRANSFORMED
;;             HelloClj.sayHelloToEveryone(names);
;;         }
;;
;;         public void sayHelloToEveryoneLoudly() {
;;             // TRANSFORMED
;;             HelloClj.sayHelloToEveryoneLoudly(names);
;;         }
;;     }
;;
;; This is the bare-bones functionality that JAll is targeting. There also need
;; to be facilities for the following:
;;
;;  * Assigning return values to Java variables
;;  * Requiring/importing packages for the AJVM (probably a special form at the top of the Java file along with regular imports)
;;  * Definining functions/methods that aren't referenced directly in the Java, but are used by other snippets (probably at the top or bottom of the file in a special form, with function/method definitions that will be added to the same class/namespace as the auto-generated one for active snippets).
;;
;; Nice-to-have's that will probably be essential once the rest of this is working:
;;
;;  * Allow users to pass in a set of options, both to individual snippet definitions and to the JAll "compiler", for anything that we're hard-coding (name of target AJVM class, whether a method is static or not, the custom "Java string" annotation that JRuby provides for method definitions, etc.)
;;  * Allow users to write pluggable transformation functions for converting to-and-from Java and the AJVM, including parameters and return values, at the barrier where AJVM snippets are called (This would be especially useful for Java objects that need a custom "serialization/deserialization" to and from the AJVM. Though of course all AJVM's provide mechanisms for easily manipulating Java objects directly, it would be more intuitive to transform them to more amenable data structures if heavy manipulation is required.).
;;
;; Work in progress.
;;
(ns jall.core
  (:use clojure.pprint)
  (:require [jall.parser :as p]
            [jall.util :as u]
            [jall.generate :as gen]
            [jall.io :as jall-io]
            [fs.core :as fs]))

(defn produce-java-support-files
  [source-file parse-tree langs]
  (let [package (p/java-package parse-tree)
        class-name (u/class-name-from-file source-file)
        full-class-name (str package "." class-name)
        methods (p/blocks-as-methods (p/blocks parse-tree))]
    (gen/output-java-support-files full-class-name methods)))

(defn produce-ajvm-files
  [source-file parse-tree]
  (let [package (p/java-package parse-tree)
        class-name (u/class-name-from-file source-file)
        full-class-name (str package "." class-name)
        methods (p/blocks-as-methods (p/blocks parse-tree))]
    (gen/output-ajvm-files full-class-name methods)))

(defn reproduce-java-file
  [source-file parse-tree langs]
  (let [package (p/java-package parse-tree)
        class-name (u/class-name-from-file source-file)
        full-class-name (str package "." class-name)]
   (gen/output-java-file full-class-name langs source-file)))

(def test-project-root "/Users/semperos/dev/java/foo")
(def test-sample-src "resources/Hello.jall")
(def test-pom "resources/jall_pom.xml")

(defn -main
  "Parse a JAll file, generate the appropriate supporting Java and AJVM code, and write to the file system."
  []
  (let [project-root test-project-root
        sample-src test-sample-src
        sample-tree (p/strict-parse sample-src)
        ajvm-files (produce-ajvm-files sample-src sample-tree)
        java-support-files (produce-java-support-files sample-src
                                                       sample-tree
                                                       (map (fn [file-record]
                                                              (:lang file-record)) ajvm-files))
        java-file  (reproduce-java-file sample-src
                                        sample-tree
                                        (map (fn [file-record]
                                               (:lang file-record)) ajvm-files))]
    (println "\n ----> CODE GENERATED <----\n")

    (println "\n ----> Java Support Interfaces <----\n")
    (doseq [file java-support-files]
      (print (:content file)))
    (println "\n ----> AJVM Files <----\n")
    (doseq [file ajvm-files]
      (print (:content file)))
    (println "\n ----> Java File <----\n")
    (print (:content java-file))
    
    (println "\n ----> WRITING OUT <----")
    
    (println "----> Writing Java Support Interfaces <----")
    (doseq [file java-support-files]
      (jall-io/prepare-and-write-file project-root file))
    (println "----> Writing AJVM Files <----")
    (doseq [file ajvm-files]
      (jall-io/prepare-and-write-file project-root file))
    (println "----> Writing Final Java File <----")
    (jall-io/prepare-and-write-file project-root java-file)
    (println "----> Writing Working POM File <----")
    (fs/copy test-pom (clojure.string/join "/" [project-root "pom.xml"]))))