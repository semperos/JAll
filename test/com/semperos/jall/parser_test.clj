(ns com.semperos.jall.parser-test
  (:use expectations.scenarios
        com.semperos.jall.parser
        [com.semperos.jall.test-utils :only [sample-src]])
  (:require [net.cgrand.parsley :as parsley]))


(def common-tree (atom nil))
(def clj-tree (atom nil))
(def rb-tree (atom nil))
(def sc-tree (atom nil))

(defn clj-parse*
  "Clojure parser (memoized)"
  []
  (if (nil? @clj-tree)
    (reset! clj-tree (strict-parse :clj sample-src))
    @clj-tree))

(defn rb-parse*
  "Ruby parser (memoized)"
  []
  (if (nil? @rb-tree)
    (reset! rb-tree (strict-parse :rb sample-src))
    @rb-tree))

(defn sc-parse*
  "Ruby parser (memoized)"
  []
  (if (nil? @sc-tree)
    (reset! sc-tree (strict-parse :sc sample-src))
    @sc-tree))

(defn common-parse*
  "Parser for common components (memoized)"
  []
  (if (nil? @common-tree)
    (reset! common-tree (common-parse sample-src))
    @common-tree))

;; ### AJVM Languages Supported ###

(scenario
 (given [check lang] (expect check (parser lang))
   (expect
    fn? :clj
    fn? :rb
    fn? :sc)))

(scenario
 (given (clj-parse*)
   (expect
    identity net.cgrand.parsley.Node
    :tag :root)))

(scenario
 ;; Strict vs Loose parsing
 (given [filter-fn pred] (expect pred
                                 (filter (fn [node]
                                           (= (:tag node) :net.cgrand.parsley/unexpected))
                                         (:content (filter-fn :clj sample-src))))
   strict-parse empty?
   loose-parse #(> (count %) 0)))

(scenario
 (given (strict-parse-str :clj (slurp sample-src))
   (expect
    identity net.cgrand.parsley.Node
    :tag :root
    empty? #(filter (fn [node]
                     (= (:tag node) :net.cgrand.parsley/unexpected))
                   (:content %)))))

(scenario
 (given (adhoc-parse (parsley/parser :list #{:word [:list "," :word]}
                                     :word #"\w+")
                     "foo,bar,baz")
   (expect
    identity net.cgrand.parsley.Node
    :tag :net.cgrand.parsley/root)))

;; ### Common Component Parsing ###

(scenario
 (given (java-package (common-parse*))
   (expect
    identity String
    identity not-empty
    identity "com.semperos")))

(scenario
 (given (java-imports (common-parse*))
   (expect
    count 3
    #(java-import-class (first %)) "java.util.List"
    #(java-import-class (second %)) "java.util.ArrayList")))


;; ### Clojure Parsing ###

;; Import nodes
(scenario
 (given (imports (clj-parse*))
   (expect
    identity not-empty
    count 1
    identity (fn [impts] (every? #(= net.cgrand.parsley.Node (type %)) impts)))))

;; Single import node
(scenario
 (given (first (imports (clj-parse*)))
   (expect
    identity net.cgrand.parsley.Node
    import-lang :clj
    #(-> % import-body read-string) '(:require [clojure.string :as string]))))

;; Import records
(scenario
 (given (blocks-as-imports (imports (clj-parse*)))
   (expect
    identity (fn [impts] (every? #(= (type %) com.semperos.jall.parser.Import) impts)) 
    count 1)))

;; State block nodes
(scenario
 (given (states (clj-parse*))
   (expect
    count 1
    identity (fn [blks] (every? #(= (type %) net.cgrand.parsley.Node) blks)))))

;; Single state block node
(scenario
 (given (first (states (clj-parse*)))
   (expect
    identity net.cgrand.parsley.Node
    state-lang :clj
    state-name String
    state-name not-empty
    state-name "nameOfState"
    state-type nil
    #(first (-> % state-body read-string)) 'ref
    #(last (-> % state-body read-string)) '{:foo "bar" :langs #{:clojure :jruby :scala}})))

;; Helper nodes
(scenario
 (given (helpers (clj-parse*))
   (expect
    identity not-empty
    count 1
    identity (fn [hlprs] (every? #(= net.cgrand.parsley.Node (type %)) hlprs)))))

;; Single Helper node
(scenario
 (given (first (helpers (clj-parse*)))
   (expect
    helper-lang :clj
    #(-> % helper-body read-string) '(defn try-me [] (println "Clojure helper function")))))

;; Helper records
(scenario
 (given (blocks-as-helpers (helpers (clj-parse*)))
   (expect
    identity (fn [hlprs] (every? #(= (type %) com.semperos.jall.parser.Helper) hlprs)) 
    count 1)))

;; Method block nodes
(scenario
 (given (methods (clj-parse*))
   (expect
    count 1
    identity (fn [blks] (every? #(= (type %) net.cgrand.parsley.Node) blks)))))

;; Single method block node
(scenario
 (given (first (methods (clj-parse*)))
   (expect
    identity net.cgrand.parsley.Node
    method-lang :clj
    method-name String
    method-name not-empty
    method-args clojure.lang.PersistentTreeMap
    method-args not-empty
    method-return-type String
    method-return-type not-empty
    #(first (-> % method-body read-string)) 'let
    #(last (-> % method-body read-string)) '(doseq [n names] (println (str "Hello, " n punctuation))))))

;; Method records
(scenario
 (given (blocks-as-methods (methods (clj-parse*)))
   (expect
    identity (fn [mthds] (every? #(= (type %) com.semperos.jall.parser.Method) mthds)))))

;; Individual Method record
(scenario
 (given (-> (clj-parse*) methods blocks-as-methods first)
   (expect
    :lang :clj
    :name "say-hello-to-everyone-loudly"
    :return-type "void"
    :body #"println"
    :args clojure.lang.PersistentTreeMap
    :args not-empty)))

;; ### Ruby Parsing ###

;; Import nodes
(scenario
 (given (imports (rb-parse*))
   (expect
    identity not-empty
    count 1
    identity (fn [impts] (every? #(= net.cgrand.parsley.Node (type %)) impts)))))

;; Single import node
(scenario
 (given (first (imports (rb-parse*)))
   (expect
    identity net.cgrand.parsley.Node
    import-lang :rb
    import-body #"nokogiri")))

;; Import records
(scenario
 (given (blocks-as-imports (imports (rb-parse*)))
   (expect
    count 1
    identity (fn [impts] (every? #(= (type %) com.semperos.jall.parser.Import) impts)))))

;; State block nodes
(scenario
 (given (states (rb-parse*))
   (expect
    count 1
    identity (fn [blks] (every? #(= (type %) net.cgrand.parsley.Node) blks)))))

;; Single state block node
(scenario
 (given (first (states (rb-parse*)))
   (expect
    identity net.cgrand.parsley.Node
    state-lang :rb
    state-name String
    state-name not-empty
    state-name "name_of_state"
    state-type nil
    state-body #":clojure, :jruby, :scala")))

;; Helper nodes
(scenario
 (given (helpers (rb-parse*))
   (expect
    identity not-empty
    count 1
    identity (fn [hlprs] (every? #(= net.cgrand.parsley.Node (type %)) hlprs)))))

;; Single Helper node
(scenario
 (given (first (helpers (rb-parse*)))
   (expect
    helper-lang :rb
    helper-body #"Ruby helper method")))

;; Helper records
(scenario
 (given (blocks-as-helpers (helpers (rb-parse*)))
   (expect
    identity (fn [hlprs] (every? #(= (type %) com.semperos.jall.parser.Helper) hlprs)) 
    count 1)))

;; Method block nodes
(scenario
 (given (methods (rb-parse*))
   (expect
    count 2
    identity (fn [blks] (every? #(= (type %) net.cgrand.parsley.Node) blks)))))

;; Single method block node
(scenario
 (given (first (methods (rb-parse*)))
   (expect
    identity net.cgrand.parsley.Node
    method-lang :rb
    method-name String
    method-name not-empty
    method-args clojure.lang.PersistentTreeMap
    method-args not-empty
    method-return-type String
    method-return-type not-empty
    method-body #"x\s+\*\s+x")))

;; Method records
(scenario
 (given (blocks-as-methods (methods (rb-parse*)))
   (expect
    identity (fn [mthds] (every? #(= (type %) com.semperos.jall.parser.Method) mthds)))))

;; Individual Method record
(scenario
 (given (-> (rb-parse*) methods blocks-as-methods first)
   (expect
    :lang :rb
    :name "square_nums"
    :return-type "Integer"
    :body #"x\s+\*\s+"
    :args clojure.lang.PersistentTreeMap
    :args not-empty)))

;; ### Scala (new) Parsing ###

;; Import nodes
(scenario
 (given (imports (sc-parse*))
   (expect
    identity not-empty
    count 1
    identity (fn [impts] (every? #(= net.cgrand.parsley.Node (type %)) impts)))))

;; Single import node
(scenario
 (given (first (imports (sc-parse*)))
   (expect
    identity net.cgrand.parsley.Node
    import-lang :sc
    import-body #"FileInputStream")))

;; Import records
(scenario
 (given (blocks-as-imports (imports (sc-parse*)))
   (expect
    count 1
    identity (fn [impts] (every? #(= (type %) com.semperos.jall.parser.Import) impts)))))

;; State block nodes
(scenario
 (given (states (sc-parse*))
   (expect
    count 1
    identity (fn [blks] (every? #(= (type %) net.cgrand.parsley.Node) blks)))))

;; Single state block node
(scenario
 (given (first (states (sc-parse*)))
   (expect
    identity net.cgrand.parsley.Node
    state-lang :sc
    state-name String
    state-name not-empty
    state-name "nameOfState"
    state-type "scala.collection.immutable.List"
    state-body #"List.*?scala")))

;; Helper nodes
(scenario
 (given (helpers (sc-parse*))
   (expect
    identity not-empty
    count 1
    identity (fn [hlprs] (every? #(= net.cgrand.parsley.Node (type %)) hlprs)))))

;; Single Helper node
(scenario
 (given (first (helpers (sc-parse*)))
   (expect
    helper-lang :sc
    helper-body #"Scala helper method")))

;; Helper records
(scenario
 (given (blocks-as-helpers (helpers (sc-parse*)))
   (expect
    identity (fn [hlprs] (every? #(= (type %) com.semperos.jall.parser.Helper) hlprs)) 
    count 1)))

;; Method block nodes
(scenario
 (given (methods (sc-parse*))
   (expect
    count 1
    identity (fn [blks] (every? #(= (type %) net.cgrand.parsley.Node) blks)))))

;; Single method block node
(scenario
 (given (first (methods (sc-parse*)))
   (expect
    identity net.cgrand.parsley.Node
    method-lang :sc
    method-name String
    method-name not-empty
    method-args clojure.lang.PersistentTreeMap
    method-args not-empty
    method-return-type String
    method-return-type not-empty
    method-body #"x\s+\*\s+\d+")))

;; Method records
(scenario
 (given (blocks-as-methods (methods (sc-parse*)))
   (expect
    identity (fn [mthds] (every? #(= (type %) com.semperos.jall.parser.Method) mthds)))))

;; Individual Method record
(scenario
 (given (-> (sc-parse*) methods blocks-as-methods first)
   (expect
    :lang :sc
    :name "timesEight"
    :return-type "Integer"
    :body #"x\s+\*\s+\d+"
    :args clojure.lang.PersistentTreeMap
    :args not-empty)))

;; ### Higher-level Parsing ###
(scenario
 (given (combine-parse-trees (common-parse*)
                             (clj-parse*)
                             (rb-parse*)
                             (sc-parse*))
   (expect
    identity net.cgrand.parsley.Node
    :tag :root
    empty? #(filter (fn [node]
               (= (:tag node) :net.cgrand.parsley/unexpected))
             %)
    identity #(not= % (clj-parse*))
    identity #(not= % (rb-parse*))
    identity #(not= % (sc-parse*))
    identity (fn [tree] (every? (fn [content-item]
                                 (some #{content-item} (:content tree)))
                               (:content (clj-parse*))))
    identity (fn [tree] (every? (fn [content-item]
                                 (some #{content-item} (:content tree)))
                               (:content (rb-parse*))))
    identity (fn [tree] (every? (fn [content-item]
                                 (some #{content-item} (:content tree)))
                               (:content (sc-parse*)))))))
