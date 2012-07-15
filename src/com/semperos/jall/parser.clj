;;   Copyright (c) Daniel Gregoire. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns com.semperos.jall.parser
  (:use [clojure.pprint :only [pprint]])
  (:require [net.cgrand.parsley :as p]
            [com.semperos.jall.util :as u]
            [clojure.string :as string]))

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

(defrecord Helper [lang body])

(defn init-helper
  ([] (init-helper nil nil))
  ([lang] (init-helper lang nil))
  ([lang body] (Helper. lang body)))

;; Main parser
(defn parser
  "Return parser for given lang"
  [lang]
  (let [class-name #"[a-zA-Z]+[a-zA-Z0-9_\.]*(?:<[a-zA-Z]+[a-zA-Z0-9_\.]*(?:<[a-zA-Z]+[a-zA-Z0-9_\.]*>)*>)*"
        java-type #"[A-Za-z]+[a-zA-Z0-9_\.]*"
        lparen #"\(\s*"
        rparen #"\)\s*"
        empty-parens "()"
        comma #",\s*"
        colon #"\s*:\s*"
        open-brackets #"(?m)\{\{\s*$"
        close-brackets #"(?m)^\s*\}\}"
        def-args [:lparen :arg-type-list :rparen]
        arg-type-list #{"" :arg-type [:arg-type-list :comma :arg-type]}
        arg-type [:identifier :colon :class-name]]
    (case lang
      :common (p/parser {:main :expr*
                         :root-tag :root}
                        ;; NOTE: The Java package and import statements need to
                        ;; be declared contiguously (white space permitted).
                        ;; Any import statements found after an `:unexpected`
                        ;; node are ignored for the purposes of JAll.
                        ;;
                        ;; This 'bug' perhaps will be a feature for folks who
                        ;; might need to 'hide' Java import statements from JAll's
                        ;; logic.
                        :expr- #{:java-package :java-import}
                        :java-package [:keywd-package #"[^;]+" #"(?s);?\s*"]
                        :keywd-package- #"^\s*package\s+"
                        :java-import [:keywd-import #"[^;]+" #"(?s);?\s*"]
                        :keywd-import- #"(?s)^import\s+")
      :clj (p/parser {:main :expr*
                      :root-tag :root}
                     :expr- #{:import-block :helper-block :code-block}
                     :import-block [:keywd-import :open-brackets :close-brackets]
                     :keywd-import #"!import_(?:clj|clojure)"
                     :open-brackets open-brackets
                     :close-brackets close-brackets
                     
                     :code-block [:open-block :close-brackets]
                     :open-block- [:def-prelude :open-brackets]
                     
                     :def-prelude- [:keywd-def :def-name :def-args :def-return-type]
                     :keywd-def #"!def_(?:clj|clojure)\s+"
                     
                     :def-name #"[a-zA-Z!\?<>_-]+[a-zA-Z0-9!\?<>_-]*"
                     
                     :def-args def-args                     
                     :lparen- lparen
                     :rparen- rparen
                     :arg-type-list- arg-type-list
                     :comma- comma
                     :arg-type- arg-type
                     :colon- colon
                     :identifier #"[a-zA-Z-\?!]+(?:(?!,\s*))*"

                     :def-return-type [#"\s*:\s*" :class-name #"\s*"]
                     :class-name class-name

                     :helper-block [:keywd-helper :open-brackets :close-brackets]
                     :keywd-helper #"!helpers?_(?:clj|clojure)")
      :rb (p/parser {:main :expr*
                     :root-tag :root}
                    :expr- #{:import-block :helper-block :code-block}
                    :import-block [:keywd-import :open-brackets :close-brackets]
                    :keywd-import #"!import_(?:rb|ruby|jruby)"
                    :open-brackets open-brackets
                    :close-brackets close-brackets
                    
                    :code-block [:open-block :close-brackets]
                    :open-block- [:def-prelude :open-brackets]
                    
                    :def-prelude- [:keywd-def :def-name :def-args :def-return-type]
                    :keywd-def #"!def_(?:rb|ruby|jruby)\s+"
                    
                    :def-name #"[a-zA-Z_]+[a-zA-Z0-9!\?_]*"
                    
                    :def-args def-args                    
                    :lparen- lparen
                    :rparen- rparen
                    :arg-type-list- arg-type-list
                    :comma- comma
                    :arg-type- arg-type
                    :colon- colon
                    :identifier #"[a-zA-Z_]+[a-zA-Z0-9!\?_]*"

                    :def-return-type [#"\s*:\s*" :class-name #"\s*"]
                    :class-name class-name
                    
                    :helper-block [:keywd-helper :open-brackets :close-brackets]
                    :keywd-helper #"!helpers?_(?:rb|ruby|jruby)")
      :sc (p/parser {:main :expr*
                     :root-tag :root}
                    :expr- #{:import-block :helper-block :code-block}
                    :import-block [:keywd-import :open-brackets :close-brackets]
                    :keywd-import #"!import_(?:sc|scala)"
                    :open-brackets open-brackets
                    :close-brackets close-brackets
                    
                    :code-block [:open-block :close-brackets]
                    :open-block- [:def-prelude :open-brackets]
                    
                    :def-prelude- [:keywd-def :def-name :def-args :def-return-type]
                    :keywd-def #"!def_(?:sc|scala)\s+"
                    
                    :def-name #"[a-zA-Z_]+[a-zA-Z0-9_]*"
                    
                    :def-args def-args
                    :lparen- lparen
                    :rparen- rparen
                    :arg-type-list- arg-type-list
                    :comma- comma
                    :arg-type- arg-type
                    :colon- colon
                    :identifier #"[a-zA-Z_]+[a-zA-Z0-9_]*"

                    :def-return-type [#"\s*:\s*" :class-name #"\s*"]
                    :class-name class-name
                    
                    :helper-block [:keywd-helper :open-brackets :close-brackets]
                    :keywd-helper #"!helpers?_(?:sc|scala)"))
    ))

(defn loose-parse
  "Parse, leave mess"
  [lang file-name]
  (let [parser (parser (keyword lang))]
    (-> (u/pbuffer-from-file parser file-name)
        p/parse-tree)))

(defn loose-parse-str
  "Parse and remove unexpected top-level things from a given input string"
  [lang input]
  (let [parser (parser (keyword lang))]
    (-> (p/incremental-buffer parser)
        (p/edit 0 0 input)
        p/parse-tree)))

(defn common-parse
  "Custom parsing logic for the `:common` parser, based on `loose-parse`"
  [file-name]
  (let [parser (parser :common)
        messy-tree (-> (u/pbuffer-from-file parser file-name)
                       p/parse-tree)
        legal-content (take-while (fn [node]
                                    (not= (:tag node) :net.cgrand.parsley/unexpected))
                                  (:content messy-tree))]
    (assoc messy-tree :content legal-content)))

(defn common-parse-str
  "Custom parsing logic for the `:common` parser, based on `loose-parse`"
  [input]
  (let [parser (parser :common)
        messy-tree (-> (p/incremental-buffer parser)
                       (p/edit 0 0 input)
                       p/parse-tree)
        legal-content (take-while (fn [node]
                                    (not= (:tag node) :net.cgrand.parsley/unexpected))
                                  (:content messy-tree))]
    (assoc messy-tree :content legal-content)))

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

(defn combine-parse-trees
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

(defn java-imports
  "Pull out the top-level Java imports in the file"
  [root-node]
  (let [content (:content root-node)]
    (filter #(= (:tag %) :java-import) content)))

(defn java-import-class
  "Given a `java-import` node, extract the value of the class being imported"
  [java-import]
  (-> java-import
      :content
      second))

(defn imports
  "Specialized JAll import for AJVM languages"
  [root-node]
  (let [contents (:content root-node)]
    (filter (fn [node] (= (:tag node) :import-block)) contents)))

(defn import-lang
  [import-node]
  (let [contents (:content import-node)
        keywd-node (first (filter (fn [node] (= (:tag node) :keywd-import)) contents))]
    (u/lang-legend (string/trim (second
                                 (re-find #"_([^_]+)$" (first (:content keywd-node))))))))

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

(defn helpers
  "Specialized JAll helper for AJVM languages"
  [root-node]
  (let [contents (:content root-node)]
    (filter (fn [node] (= (:tag node) :helper-block)) contents)))

(defn helper-lang
  [helper-node]
  (let [contents (:content helper-node)
        keywd-node (first (filter (fn [node] (= (:tag node) :keywd-helper)) contents))]
    (u/lang-legend (string/trim (second
                                 (re-find #"_([^_]+)$" (first (:content keywd-node))))))))

(defn helper-body
  [helper-node]
  (let [contents (:content helper-node)
        ajvm-codes (filter #(= (:tag %) :net.cgrand.parsley/unexpected) contents)]
    (apply str (map #(-> % :content first) ajvm-codes))))

(defn blocks-as-helpers
  "Create `Helper` records out of :helper-block blocks"
  [helpers]
  (for [helper helpers]
    (init-helper (helper-lang helper)
                 (helper-body helper))))

(defn blocks
  "Given root parse-tree node, return all code blocks"
  [root-node]
  (let [contents (:content root-node)
        all-blocks (filter (fn [node] (= (:tag node) :code-block)) contents)]
    (remove (fn [block]
              (= (:tag (second (:content block)))
                 :net.cgrand.parsley/unexpected))
            all-blocks)))

(defn block-lang
  "Return keyword for language code of given code block, e.g., `:clj`

   In order to make the parser unambiguous, we have to hard-code a language-specific path at some point. This is that point, which is why the below code looks ugly."
  [node]
  (let [content (:content node)
        keywd-node (first (filter (fn [node] (= (:tag node) :keywd-def)) content))]
    (u/lang-legend (string/trim (second
                                 (re-find #"_([^_]+)$" (first (:content keywd-node))))))))

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