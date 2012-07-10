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
  "Return parser for given lang"
  [lang]
  (let [class-name #"[a-zA-Z]+[a-zA-Z0-9_\.]*(?:<[a-zA-Z]+[a-zA-Z0-9_\.]*(?:<[a-zA-Z]+[a-zA-Z0-9_\.]*>)*>)*"
        java-type #"[A-Za-z]+[a-zA-Z0-9_\.]*"
        keywd-import #"!import\s+"
        keywd-def #"!def\s+"
        lparen #"\(\s*"
        rparen #"\)\s*"
        comma #",\s*"
        colon #"\s*:\s*"
        open-brackets #"(?m)\{\{\s*$"
        close-brackets #"(?m)^\s*\}\}"]
      (case lang
        :common (p/parser {:main :expr*
                           :root-tag :root}
                          :expr- #{:java-package}
                          :java-package [:keywd-package #"[^;]+" #";?"]
                          :keywd-package- #"^\s*package\s+")
        :clj (p/parser {:main :expr*
                        ;; :space :ws?
                        :root-tag :root}
                       ;; :ws #"\s+"
                       :expr- #{:import-block :code-block}
                       :import-block [:keywd-import :lang :open-brackets :close-brackets]
                       :keywd-import keywd-import
                       :lang "clj"
                       :open-brackets open-brackets
                       :close-brackets close-brackets
               
                       :code-block [:open-block :close-brackets]
                       :open-block- [:def-prelude :open-brackets]
               
                       :def-prelude- [:keywd-def :lang-prefix :def-name :def-return-type :def-args]
                       :keywd-def keywd-def
                       :lang-prefix ["clj" "_"]
               
                       :def-name #"[a-zA-Z!\?<>_-]+[a-zA-Z0-9!\?<>_-]*"
                       ;; :jruby-def-name- #"[a-zA-Z_]+[a-zA-Z0-9!\?_]*"
               
                       :def-return-type [#"\s*:\s*" :class-name #"\s*"]
                       :class-name class-name
               
                       :def-args [:lparen :arg-type-list :rparen]
               
                       :lparen- lparen
                       :rparen- rparen
                       :arg-type-list- #{:arg-type [:arg-type-list :comma :arg-type]}
                       :comma- comma
                       :arg-type- [:clj-symbol :colon :class-name]
                       :colon- colon
                       :clj-symbol #"[a-zA-Z-\?!]+(?:(?!,\s*))*"
                       ;; Currently hard-coded to only handle generics two-deep, e.g., `List<List<String>>`
                       ;; Feel like this is similar to clj-arg-list and children...
                ))))

(defn loose-parse
  "Parse, leave mess"
  [lang file-name]
  (let [parser (parser (keyword lang))]
    (-> (u/pbuffer-from-file parser file-name)
        p/parse-tree)))

(defn strict-parse
  "Parse and remove unexpected top-level things for the given AJVM `lang`"
  [lang file-name]
  (let [parser (parser (keyword lang))]
    (-> (u/pbuffer-from-file parser file-name)
        p/parse-tree
        u/remove-top-unexpected)))

(defn strict-parse-str
  "Parse and remove unexpected top-level things from a given input string"
  [lang input]
  (let [parser (parser (keyword lang))]
    (-> (p/incremental-buffer parser)
        (p/edit 0 0 input)
        p/parse-tree
        u/remove-top-unexpected)))

(defn adhoc-parse
  [parser input]
  (-> (p/incremental-buffer parser)
      (p/edit 0 0 input)
      p/parse-tree))

(defn tend-trees
  "Given a `common-tree` parsing of the common parser and all other trees, merge 'em"
  [common-tree & trees]
  (reduce (fn [state tree]
            (update-in state [:content] concat (:content tree)))
          common-tree
          trees))

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
    (keyword (first (:content lang-node)))))

(defn import-body
  [import-node]
  (let [contents (:content import-node)
        ajvm-codes (filter #(= (:tag %) :net.cgrand.parsley/unexpected) contents)]
    (apply str (map #(-> % :content first) ajvm-codes))))

(defn blocks-as-imports
  "Create `Import` records out of :import-block blocks"
  [imports]
  (for [import imports]
    (init-import (import-lang import)
                 (import-body import))))

(defn blocks
  "Given root parse-tree node, return all code blocks"
  [root-node]
  (let [contents (:content root-node)]
    (filter (fn [node] (= (:tag node) :code-block)) contents)))

(defn block-lang
  "Return keyword for language code of given code block, e.g., `:clj`

   In order to make the parser unambiguous, we have to hard-code a language-specific path at some point. This is that point, which is why the below code looks ugly."
  [node]
  (let [content (:content node)
        lang-node (first (filter (fn [node] (= (:tag node) :lang-prefix)) content))]
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