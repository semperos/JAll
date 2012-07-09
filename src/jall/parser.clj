(ns jall.parser
  (:use [clojure.pprint :only [pprint]])
  (:require [net.cgrand.parsley :as p]
            [jall.util :as u]))

(defrecord Method [lang name args return-type body])

(defn init-method
  "Initialize a new `Method` record"
  ([] (init-method nil nil nil nil nil))
  ([lang] (init-method lang nil nil nil nil))
  ([lang name] (init-method lang name nil nil nil))
  ([lang name args] (init-method lang name args nil nil))
  ([lang name args return-type] (init-method lang name args return-type nil))
  ([lang name args return-type body] (Method. lang name args return-type body)))

(defrecord Import [lang body])

(defn init-import
  ([] (init-import nil nil))
  ([lang] (init-import lang nil))
  ([lang body] (Import. lang body)))

;; Main parser
(defn parser
  "Wrap parser in a fn"
  []
  (let [jall-parser (p/parser {:main :expr*
                           ;; :space :ws?
                           :root-tag :root}
                          ;; :ws #"\s+"
                          :expr- #{:java-package :import-block :code-block}
                          :java-package [:keywd-package #"[^;]+" #";?"]
                          :keywd-package- #"^\s*package\s+"

                          :import-block [:keywd-import :lang :open-brackets :close-brackets]
                          :keywd-import #"!import\s+"
                          :lang #{:clj-abbr :jruby-abbr :scala-abbr}
                          :clj-abbr- "clj"
                          :jruby-abbr- "rb"
                          :scala-abbr- "sc"
                          :open-brackets #"(?m)\{\{\s*$"
                          :close-brackets #"(?m)^\s*\}\}"
                
                          :code-block [:open-block :close-brackets]
                          :open-block- [:def-prelude :open-brackets]

                          :def-prelude- [:keywd-def :lang-prefix :def-name :def-return-type :def-args]
                          :keywd-def #"!def\s+"
                          :lang-prefix #{[:clj-abbr "_"] [:jruby-abbr "_"] [:scala-abbr "_"]}

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
                          ;; Currently hard-coded to only handle generics two-deep, e.g., `List<List<String>>`
                          ;; Feel like this is similar to clj-arg-list and children...
                          :class-name #"[a-zA-Z]+[a-zA-Z0-9_\.]*(?:<[a-zA-Z]+[a-zA-Z0-9_\.]*(?:<[a-zA-Z]+[a-zA-Z0-9_\.]*>)*>)*")]
    jall-parser))

(defn loose-parse
  "Parse, leave mess"
  [file-name]
  (let [parser (parser)]
    (-> (u/pbuffer-from-file parser file-name)
        p/parse-tree)))

(defn strict-parse
  "Parse and remove unexpected top-level things"
  [file-name]
  (let [parser (parser)]
    (-> (u/pbuffer-from-file parser file-name)
        p/parse-tree
        u/remove-top-unexpected)))

(defn strict-parse-str
  "Parse and remove unexpected top-level things from a given input string"
  [input]
  (let [parser (parser)]
    (-> (p/incremental-buffer parser)
        (p/edit 0 0 input)
        p/parse-tree
        u/remove-top-unexpected)))

(defn adhoc-parse
  [parser input]
  (-> (p/incremental-buffer parser)
      (p/edit 0 0 input)
      p/parse-tree))

(defn java-package
  "Pull out the package of the file being parsed, used to create AJVM classes"
  [root-node]
  (let [content (:content root-node)
        java-package-node (first (filter #(= (:tag %) :java-package) content))]
    (-> java-package-node
        :content
        second)))

(defn imports
  "Specialized JAll import for AJVM languages"
  [root-node]
  (let [contents (:content root-node)]
    (filter (fn [node] (= (:tag node) :import-block)) contents)))

(defn import-lang
  [import-node]
  (let [contents (:content import-node)
        lang-node (first (filter (fn [node] (= (:tag node) :lang)) contents))]
    (first (:content lang-node))))

(defn import-body
  [import-node]
  (let [contents (:content import-node)
        ajvm-codes (filter #(= (:tag %) :net.cgrand.parsley/unexpected) contents)]
    (apply str (map #(-> % :content first) ajvm-codes))))

(defn blocks-as-imports
  "Create `Import` records out of :import-block blocks"
  [imports]
  (for [import imports]
    (init-import (keyword (import-lang import))
                 (import-body import))))

(defn blocks
  "Given root parse-tree node, return all code blocks"
  [root-node]
  (let [contents (:content root-node)]
    (filter (fn [node] (= (:tag node) :code-block)) contents)))

(defn block-lang
  "Return keyword for language code of given code block, e.g., `:clj`"
  [node]
  (let [content (:content node)
        lang-node (first (filter #(= (:tag %) :lang-prefix) content))]
    (keyword (-> lang-node :content first))))

(defn block-method-name
  "Return name of method for code block"
  [node]
  (let [content (:content node)
        def-name-node (first (filter #(= (:tag %) :def-name) content))]
    (-> def-name-node :content first)))

(defn block-method-args
  "Get the args for the code block's method"
  [node]
  (let [content (:content node)
        args-node (first (filter #(= (:tag %) :def-args) content))]
    (apply sorted-map (map #(-> % :content first) (u/clean-syntactic-cruft args-node)))))

(defn block-return-type
  "Get the return type for the block method"
  [node]
  (let [lang (block-lang node)
        content (:content node)
        return-type-node (first (filter #(= (:tag %) :def-return-type) content))
        return-type (-> (u/clean-syntactic-cruft return-type-node)
                        first
                        :content
                        first)]
    return-type))

(defn block-body
  "Extract all the code inside the method def for a given code block, which we keep track of by not parsing it at all :-)"
  [node]
  {:pre [(= (:tag (last (:content node))) :close-brackets)]}
  (let [content (:content node)
        ajvm-codes (filter #(= (:tag %) :net.cgrand.parsley/unexpected) content)]
    (apply str (map #(-> % :content first) ajvm-codes))))

(defn blocks-as-methods
  "Given all code block nodes from a given parse tree, create the appropriate `Method` records."
  [blocks]
  (for [block blocks]
    (init-method (block-lang block)
                 (block-method-name block)
                 (block-method-args block)
                 (block-return-type block)
                 (block-body block))))