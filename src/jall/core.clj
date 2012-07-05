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
  (:require [net.cgrand.parsley :as p]
            [net.cgrand.parsley.views :as v]
            [net.cgrand.parsley.tree :as t]
            [jall.util :as u]
            [jall.generate :as gen]
            [jall.io :as jall-io]
            [clojure.java.io :as io]
            [fs.core :as fs]))

(defrecord Method [lang name args return-type body])

(defn init-method
  "Initialize a new `Method` record"
  ([] (init-method nil nil nil nil nil))
  ([lang] (init-method lang nil nil nil nil))
  ([lang name] (init-method lang name nil nil nil))
  ([lang name args] (init-method lang name args nil nil))
  ([lang name args return-type] (init-method lang name args return-type nil))
  ([lang name args return-type body] (Method. lang name args return-type body)))

;; Main parser
(def jall-parser (p/parser {:main :expr*
                     ;; :space :ws?
                     :root-tag :root}
                ;; :ws #"\s+"
                :expr- #{:java-package :code-block}
                :java-package [:keywd-package #"[^;]+" #";?"]
                :keywd-package- #"^\s*package\s+"
                
                :code-block [:open-block :close-block]
                :open-block- [:def-prelude :open-brackets]
                :open-brackets #"(?m)\{\{\s*$"
                :close-block #"(?m)^\s*\}\}"

                :def-prelude- [:keywd-def :lang :def-name :def-return-type :def-args]
                :keywd-def #"!def\s+"
                :lang #{:clj-abbr :jruby-abbr :scala-abbr}
                :clj-abbr- "clj_"
                :jruby-abbr- "rb_"
                :scala-abbr- "sc_"
                :def-name #{:clj-def-name}
                :clj-def-name- #"[a-zA-Z!\?<>_-]+[a-zA-Z0-9!\?<>_-]*"
                
                :def-return-type [#"\s*:\s*" :java-type #"\s*"]
                :java-type #"[A-Za-z]+[a-zA-Z0-9_\.]*"
                
                :def-args #{:clj-arg-list}

                :clj-arg-list- [:lparen :arg-type-list :rparen]
                :lparen- #"\(\s*"
                :rparen- #"\)\s*"
                :arg-type-list- #{:arg-type [:arg-type-list :comma :arg-type]}
                :comma- #",\s*"
                :arg-type- [:clj-symbol #"\s*:\s*" :class-name]
                :clj-symbol #"[a-zA-Z-\?!]+(?:(?!,\s*))*"
                :class-name #"[a-zA-Z]+[a-zA-Z0-9_\.]*(?:<[a-zA-Z]+[a-zA-Z0-9_\.]*(?:<[a-zA-Z]+[a-zA-Z0-9_\.]*>)*>)*"))

(defn loose-parse
  "Parse, leave mess"
  [parser file-name]
  (-> (u/pbuffer-from-file parser file-name)
      p/parse-tree))

(defn strict-parse
  "Parse and remove unexpected top-level things"
  [parser file-name]
  (-> (u/pbuffer-from-file parser file-name)
      p/parse-tree
      u/remove-top-unexpected))

(defn lang-type-legend
  "Mapping of types between Java and other JVM languages. This is really just for things like void, null, nil, etc."
  [lang java-type]
  (let [lang-legend {:clj {"void" "nil"}}
        entry (get-in lang-legend [lang java-type])]
    (if (nil? entry)
      java-type
      entry)))

(defn code-java-package
  "Pull out the package of the file being parsed, used to create AJVM classes"
  [root-node]
  (let [content (:content root-node)
        java-package-node (first (filter #(= (:tag %) :java-package) content))]
    (-> java-package-node
        :content
        second)))

(defn code-blocks
  "Given root parse-tree node, return all code blocks"
  [root-node]
  (let [contents (:content root-node)]
    (filter (fn [node] (= (:tag node) :code-block)) contents)))

(defn code-block-lang
  "Return keyword for language code of given code block, e.g., `:clj`"
  [node]
  (let [content (:content node)
        lang-node (first (filter #(= (:tag %) :lang) content))]
    (keyword (second (re-find #"([^_]+)_$" (-> lang-node :content first))))))

(defn code-block-method-name
  "Return name of method for code block"
  [node]
  (let [content (:content node)
        def-name-node (first (filter #(= (:tag %) :def-name) content))]
    (-> def-name-node :content first)))

(defn code-block-method-args
  "Get the args for the code block's method"
  [node]
  (let [content (:content node)
        args-node (first (filter #(= (:tag %) :def-args) content))]
    (apply sorted-map (map #(-> % :content first) (u/clean-syntactic-cruft args-node)))))

(defn code-block-return-type
  "Get the return type for the block method"
  [node]
  (let [lang (code-block-lang node)
        content (:content node)
        return-type-node (first (filter #(= (:tag %) :def-return-type) content))
        return-type (-> (u/clean-syntactic-cruft return-type-node)
                        first
                        :content
                        first)]
    return-type))

(defn code-block-body
  "Extract all the code inside the method def for a given code block, which we keep track of by not parsing it at all :-)"
  [node]
  {:pre [(= (:tag (last (:content node))) :close-block)]}
  (let [content (:content node)
        ajvm-codes (filter #(= (:tag %) :net.cgrand.parsley/unexpected) content)]
    (apply str (map #(-> % :content first) ajvm-codes))))

(defn code-blocks-as-methods
  "Given all code block nodes from a given parse tree, create the appropriate `Method` records."
  [blocks]
  (for [block blocks]
    (init-method (code-block-lang block)
                 (code-block-method-name block)
                 (code-block-method-args block)
                 (code-block-return-type block)
                 (code-block-body block))))

(defn produce-ajvm-files
  [source-file parse-tree]
  (let [package (code-java-package parse-tree)
        class-name (u/class-name-from-file source-file)
        full-class-name (str package "." class-name)
        methods (code-blocks-as-methods (code-blocks parse-tree))]
    (gen/output-ajvm-files full-class-name methods)))

(defn produce-java-support-files
  [source-file parse-tree langs]
  (let [package (code-java-package parse-tree)
        class-name (u/class-name-from-file source-file)
        full-class-name (str package "." class-name)
        methods (code-blocks-as-methods (code-blocks parse-tree))]
    (gen/output-java-support-files full-class-name methods)))

(defn produce-java-file
  [source-file parse-tree langs]
  (let [package (code-java-package parse-tree)
        class-name (u/class-name-from-file source-file)
        full-class-name (str package "." class-name)]
   (gen/output-java-file full-class-name langs source-file)))

(defn write-file
  "Given a `File` record, write the file to the file system.

   A `File` record contains the language, full class name and content of a given file, which should be everything needed to create the necessary directory structure and write the final file."
  [file-record]
  (jall-io/prepare-and-write-file file-record))

;;
;; TODO
;; We have to generate a separate Java interface for all the methods we define
;; in Clojure, so that they can have the proper Generics, etc.
;;
;; Generated Java is already going to target/src/main/java/..., so we will put
;; these interfaces directly into the regular src/main/java/... folder.
;;
;; In long run, may want to reverse this, so that folks have to specify a special
;; JAll Maven profile to do the initial compilation, but can then interact with
;; the generated Java in src/main/java as they usually would.
;;
;; Pretty sure that previous paragraph isn't possible; time to sleep :)
;;
(defn -main
  "Parse a JAll file, generate the appropriate AJVM and Java file output."
  []
  (let [project-root "/Users/semperos/dev/java/foo"
        sample-src "resources/Hello.jall"
        sample-tree (strict-parse jall-parser sample-src)
        ajvm-files (produce-ajvm-files sample-src sample-tree)
        java-support-files (produce-java-support-files sample-src
                                                       sample-tree
                                                       (map (fn [file-record]
                                                              (:lang file-record)) ajvm-files))
        java-file  (produce-java-file sample-src
                                      sample-tree
                                      (map (fn [file-record]
                                             (:lang file-record)) ajvm-files))]
    (println "\n ----> AJVM Files <----\n")
    (doseq [file ajvm-files]
      (print (:content file)))
    (println "\n ----> Java Support Interfaces <----\n")
    (doseq [file java-support-files]
      (print (:content file)))
    (println "\n ----> Java File <----\n")
    (print (:content java-file))
    (println "\n ----> TIME TO WRITE IT OUT <----")
    (println "\n ----> Writing AJVM Files <----")
    (doseq [file ajvm-files]
      (jall-io/prepare-and-write-file project-root file))
    (println "\n ----> Writing Java Support Interfaces <----\n")
    (doseq [file java-support-files]
      (jall-io/prepare-and-write-file project-root file))
    (println "\n ----> Writing Final Java File <----")
    (jall-io/prepare-and-write-file project-root java-file)
    (println "\n ----> Write POM File <----")
    (fs/copy "resources/jall_pom.xml" (clojure.string/join "/" [project-root "pom.xml"]))))