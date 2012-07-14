(ns com.semperos.jall.parser-test
  (:use expectations
        com.semperos.jall.parser)
  (:require [expectations.scenarios :as sc]
            [net.cgrand.parsley :as parsley]))

(def sample-src "resources/Hello.jall")
(def clj-tree (atom nil))
(def common-tree (atom nil))
(defn clj-parse
  "Clojure parser (memoized)"
  []
  (if (nil? @clj-tree)
    (reset! clj-tree (strict-parse :clj sample-src))
    @clj-tree))

(defn common-parse
  "Parser for common components (memoized)"
  []
  (if (nil? @common-tree)
    (reset! common-tree (strict-parse :common sample-src))
    @common-tree))

;; ### Parser Basics ###
(expect IllegalArgumentException (parser :scala)) ;; todo
(expect fn? (parser :clj)) ;; that's right
(expect fn? (parser :rb)) ;; JRuby too

;; ### Parsing in Parts ###

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

;; #### JAll Parsing ####

;; Common components
(expect String
        (java-package (common-parse)))
(expect not-empty
        (java-package (common-parse)))
(expect "com.example" (java-package (common-parse)))

;; Imports
(expect not-empty
        (imports (clj-parse)))
(expect 1
        (count (imports (clj-parse))))
(expect :clj
        (-> (clj-parse) imports first import-lang))
(expect '(:require [clojure.string :as string])
        (-> (clj-parse) imports first import-body read-string))
(expect jall.parser.Import
        (first (blocks-as-imports (imports (clj-parse)))))
(expect 1
        (count (blocks-as-imports (imports (clj-parse)))))

;; Code blocks
(expect 2
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
(expect '(doseq [n arr-of-names] (println (str "Hello, " n)))
        (-> (clj-parse) blocks first block-body read-string))
(expect jall.parser.Method
        (-> (clj-parse) blocks blocks-as-methods first))
(given (-> (clj-parse) blocks blocks-as-methods first)
  (expect
   :lang :clj
   :name "say-hello-to-everyone"
   :return-type "void"
   :body #"println"
   :args clojure.lang.PersistentTreeMap
   :args not-empty))
