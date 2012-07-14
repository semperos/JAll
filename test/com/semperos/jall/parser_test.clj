(ns com.semperos.jall.parser-test
  (:use expectations
        com.semperos.jall.parser)
  (:require [expectations.scenarios :as sc]
            [net.cgrand.parsley :as parsley]))

(def sample-src "resources/Sample.jall")
(def common-tree (atom nil))
(def clj-tree (atom nil))
(def rb-tree (atom nil))

(defn clj-parse
  "Clojure parser (memoized)"
  []
  (if (nil? @clj-tree)
    (reset! clj-tree (strict-parse :clj sample-src))
    @clj-tree))

(defn rb-parse
  "Ruby parser (memoized)"
  []
  (if (nil? @rb-tree)
    (reset! rb-tree (strict-parse :rb sample-src))
    @rb-tree))

(defn common-parse
  "Parser for common components (memoized)"
  []
  (if (nil? @common-tree)
    (reset! common-tree (strict-parse :common sample-src))
    @common-tree))

;; ### AJVM Languages Supported ###

(expect IllegalArgumentException (parser :scala)) ;; todo
(expect fn? (parser :clj)) ;; that's right
(expect fn? (parser :rb)) ;; JRuby too

;; Basic Success
(expect net.cgrand.parsley.Node (clj-parse))
(expect :root (:tag (clj-parse)))

;; Strict parsing should remove top-level unexpected nodes
(expect empty?
        (filter (fn [node] (= (:tag node) :net.cgrand.parsley/unexpected)) (:content (clj-parse))))

;; The opposite is true for "loose" parsing
(expect (fn [coll] (> (count coll) 0))
        (filter (fn [node] (= (:tag node) :net.cgrand.parsley/unexpected)) (:content (loose-parse :clj sample-src))))

;; Also support parsing strings directly
(given (slurp sample-src)
  (expect
   (fn [s] (strict-parse-str :clj s)) net.cgrand.parsley.Node
   (fn [s] (:tag (strict-parse-str :clj s))) :root
   (fn [s] (filter
           (fn [node] (= (:tag node) :net.cgrand.parsley/unexpected))
           (:content (strict-parse-str :clj s)))) empty?))

;; For testing purposes, we keep an adhoc-parse around that uses a buffer
(expect net.cgrand.parsley.Node (adhoc-parse (parsley/parser :list #{:word [:list "," :word]}
                                                             :word #"\w+")
                                             "foo,bar,baz"))

;; ### Common Component Parsing ###

(expect String
        (java-package (common-parse)))
(expect not-empty
        (java-package (common-parse)))
(expect "com.semperos" (java-package (common-parse)))

;; ### Clojure Parsing ###

;; Imports
(expect not-empty
        (imports (clj-parse)))
(expect 1
        (count (imports (clj-parse))))
(expect :clj
        (-> (clj-parse) imports first import-lang))
(expect '(:require [clojure.string :as string])
        (-> (clj-parse) imports first import-body read-string))
(expect com.semperos.jall.parser.Import
        (first (blocks-as-imports (imports (clj-parse)))))
(expect 1
        (count (blocks-as-imports (imports (clj-parse)))))

;; Helpers
(expect not-empty
        (helpers (clj-parse)))
(expect 1
        (count (helpers (clj-parse))))
(expect :clj
        (-> (clj-parse) helpers first helper-lang))
(expect '(defn try-me [] (println "Clojure helper function"))
        (-> (clj-parse) helpers first helper-body read-string))
(expect com.semperos.jall.parser.Helper
        (first (blocks-as-helpers (helpers (clj-parse)))))
(expect 1
        (count (blocks-as-helpers (helpers (clj-parse)))))

;; Method blocks
(expect 1
        (count (blocks (clj-parse))))
(expect :clj
        (-> (clj-parse) blocks first block-lang))
(expect String
        (-> (clj-parse) blocks first block-method-name))
(expect not-empty
        (-> (clj-parse) blocks first block-method-name))
(expect clojure.lang.PersistentTreeMap
        (-> (clj-parse) blocks first block-method-args))
(expect not-empty
        (-> (clj-parse) blocks first block-method-args))
(expect String
        (-> (clj-parse) blocks first block-return-type))
(expect not-empty
        (-> (clj-parse) blocks first block-return-type))
(expect 'let
        (first (-> (clj-parse) blocks first block-body read-string)))
(expect '(doseq [n names] (println (str "Hello, " n punctuation)))
        (last (-> (clj-parse) blocks first block-body read-string)))
(expect com.semperos.jall.parser.Method
        (-> (clj-parse) blocks blocks-as-methods first))
(given (-> (clj-parse) blocks blocks-as-methods first)
  (expect
   :lang :clj
   :name "say-hello-to-everyone-loudly"
   :return-type "void"
   :body #"println"
   :args clojure.lang.PersistentTreeMap
   :args not-empty))

;; ### Ruby Parsing ###

;; Imports
(expect not-empty
        (imports (rb-parse)))
(expect 1
        (count (imports (rb-parse))))
(expect :rb
        (-> (rb-parse) imports first import-lang))
(expect #"nokogiri"
        (-> (rb-parse) imports first import-body))
(expect com.semperos.jall.parser.Import
        (first (blocks-as-imports (imports (rb-parse)))))
(expect 1
        (count (blocks-as-imports (imports (rb-parse)))))
(expect #(not (nil? (:lang %))) (first (blocks-as-imports (imports (rb-parse)))))
(expect #(not (nil? (:body %))) (first (blocks-as-imports (imports (rb-parse)))))

;; Helpers
(expect not-empty
        (helpers (rb-parse)))
(expect 1
        (count (helpers (rb-parse))))
(expect :rb
        (-> (rb-parse) helpers first helper-lang))
(expect #"Ruby helper method"
        (-> (rb-parse) helpers first helper-body))
(expect com.semperos.jall.parser.Helper
        (first (blocks-as-helpers (helpers (rb-parse)))))
(expect 1
        (count (blocks-as-helpers (helpers (rb-parse)))))
(expect #(not (nil? (:lang %))) (first (blocks-as-helpers (helpers (rb-parse)))))
(expect #(not (nil? (:body %))) (first (blocks-as-helpers (helpers (rb-parse)))))


;; Method blocks
(expect 2
        (count (blocks (rb-parse))))
(expect :rb
        (-> (rb-parse) blocks first block-lang))
(expect String
        (-> (rb-parse) blocks first block-method-name))
(expect not-empty
        (-> (rb-parse) blocks first block-method-name))
(expect clojure.lang.PersistentTreeMap
        (-> (rb-parse) blocks first block-method-args))
(expect not-empty
        (-> (rb-parse) blocks first block-method-args))
(expect String
        (-> (rb-parse) blocks first block-return-type))
(expect not-empty
        (-> (rb-parse) blocks first block-return-type))
(expect #"x\s+\*\s+x"
        (-> (rb-parse) blocks first block-body))
(expect com.semperos.jall.parser.Method
        (-> (rb-parse) blocks blocks-as-methods first))
(given (-> (rb-parse) blocks blocks-as-methods first)
  (expect
   :lang :rb
   :name "square_nums"
   :return-type "Integer"
   :body #"x\s+\*\s+"
   :args clojure.lang.PersistentTreeMap
   :args not-empty))