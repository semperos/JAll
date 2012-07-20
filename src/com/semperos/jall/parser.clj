;;   Copyright (c) Daniel Gregoire. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns com.semperos.jall.parser
  (:refer-clojure :exclude [methods])
  (:use [clojure.pprint :only [pprint]])
  (:require [net.cgrand.parsley :as p]
            [com.semperos.jall.util :as u]
            [clojure.string :as string]))

(defrecord Import [lang body])

(defn init-import
  ([] (init-import nil nil))
  ([lang] (init-import lang nil))
  ([lang body] (Import. lang body)))

(defrecord State [lang name type body])

(defn init-state
  "Initialize a new `State` record"
  ([] (init-state nil nil nil nil))
  ([lang] (init-state lang nil nil nil))
  ([lang name] (init-state lang name nil nil))
  ([lang name type] (init-state lang name type nil))
  ([lang name type body] (State. lang name type body)))

(defrecord Helper [lang body])

(defn init-helper
  ([] (init-helper nil nil))
  ([lang] (init-helper lang nil))
  ([lang body] (Helper. lang body)))

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
(defn parser
  "Return parser for given lang"
  [lang]
  (let [;; shameless
        class-name      #"[a-zA-Z]+[a-zA-Z0-9_\.]*(?:<[a-zA-Z]+[a-zA-Z0-9_\.]*(?:<[a-zA-Z]+[a-zA-Z0-9_\.]*>)*>)*"
        ;; equally shameless
        scala-class-name #"[a-zA-Z]+[a-zA-Z0-9_\.]*(?:\[[a-zA-Z]+[a-zA-Z0-9_\.]*(?:\[[a-zA-Z]+[a-zA-Z0-9_\.]*\])*\])*"
        lparen          #"\(\s*"
        rparen          #"\)\s*"
        empty-parens    "()"
        comma           #",\s*"
        colon           #"\s*:\s*"
        open-brackets   #"(?m)\s*\{\{\s*$"
        close-brackets  #"(?m)^\s*\}\}\s*$"
        method-args [:lparen :arg-type-list :rparen]
        arg-type-list #{"" :arg-type [:arg-type-list :comma :arg-type]}
        arg-type [:identifier :colon :class-name]]
    (case lang
      :common (p/parser {:main :expr*
                         :root-tag :root}
                        ;; NOTE: The Java package and import statements need to
                        ;; be declared contiguously (white space and comments permitted).
                        ;; Any import statements found after an `:unexpected`
                        ;; node are ignored for the purposes of JAll.
                        ;;
                        ;; This 'bug' perhaps will be a feature for folks who
                        ;; might need to 'hide' Java import statements from JAll's
                        ;; logic.
                        :expr- #{:java-package :java-import :java-comment}
                        :java-package [:keywd-package #"[^;]+" #"(?s);?\s*"]
                        :keywd-package- #"^\s*package\s+"
                        :java-import [:keywd-import #"(?:static\s+)?[^;]+" #"(?s);?\s*"]
                        :keywd-import- #"(?s)^\s*import\s+"
                        :java-comment #{:java-line-comment [:java-comment-start :java-comment-end]}
                        :java-line-comment- #"//.*"
                        :java-comment-start- #"\s*/\*+"
                        :java-comment-end- #"\s*\*/\s*")
      :clj (p/parser {:main :expr*
                      :root-tag :root}
                     :expr- #{:import-block :state-block :helper-block :method-block}
                     :import-block [:keywd-import :open-brackets :close-brackets]
                     :keywd-import #"\$import_(?:clj|clojure)"
                     :open-brackets open-brackets
                     :close-brackets close-brackets

                     :state-block [:open-state :close-brackets]
                     :open-state- [:state-prelude :open-brackets]
                     :state-prelude- [:keywd-state :state-args]
                     :keywd-state #"\$state_(?:clj|clojure)\s*"
                     ;; Because Scala's is a little more complex
                     :state-args :identifier

                     :helper-block [:keywd-helper :open-brackets :close-brackets]
                     :keywd-helper #"\$helpers?_(?:clj|clojure)"

                     :method-block [:open-method :close-brackets]
                     :open-method- [:method-prelude :open-brackets]

                     :method-prelude- [:keywd-def :method-name :method-args :method-return-type]
                     :keywd-def #"\$def_(?:clj|clojure)\s+"

                     :method-name #"[a-zA-Z!\?<>_-]+[a-zA-Z0-9!\?<>_-]*"

                     :method-args method-args
                     :lparen- lparen
                     :rparen- rparen
                     :arg-type-list- arg-type-list
                     :comma- comma
                     :arg-type- arg-type
                     :colon- colon
                     ;; :identifier #"[a-zA-Z-\?!]+(?:(?!,\s*))*"
                     :identifier #"[a-zA-Z!\?<>_-]+[a-zA-Z0-9!\?<>_-]*(?:(?!,\s*))*"

                     :method-return-type [#"\s*:\s*" :class-name #"\s*"]
                     :class-name class-name)
      :rb (p/parser {:main :expr*
                     :root-tag :root}
                    :expr- #{:import-block :state-block :helper-block :method-block}
                    :import-block [:keywd-import :open-brackets :close-brackets]
                    :keywd-import #"\$import_(?:rb|ruby|jruby)"
                    :open-brackets open-brackets
                    :close-brackets close-brackets

                    :state-block [:open-state :close-brackets]
                    :open-state- [:state-prelude :open-brackets]
                    :state-prelude- [:keywd-state :state-args]
                    :keywd-state #"\$state_(?:rb|ruby|jruby)\s*"
                    ;; Because Scala's is a little more complex
                    :state-args :identifier

                    :helper-block [:keywd-helper :open-brackets :close-brackets]
                    :keywd-helper #"\$helpers?_(?:rb|ruby|jruby)"
                    
                    :method-block [:open-method :close-brackets]
                    :open-method- [:method-prelude :open-brackets]

                    :method-prelude- [:keywd-def :method-name :method-args :method-return-type]
                    :keywd-def #"\$def_(?:rb|ruby|jruby)\s+"

                    :method-name #"[a-zA-Z_]+[a-zA-Z0-9!\?_]*"

                    :method-args method-args
                    :lparen- lparen
                    :rparen- rparen
                    :arg-type-list- arg-type-list
                    :comma- comma
                    :arg-type- arg-type
                    :colon- colon
                    :identifier #"[a-zA-Z_]+[a-zA-Z0-9!\?_]*"

                    :method-return-type [#"\s*:\s*" :class-name #"\s*"]
                    :class-name class-name)
      :sc (p/parser {:main :expr*
                     :root-tag :root}
                    :expr- #{:import-block :state-block :helper-block :method-block}
                    :import-block [:keywd-import :open-brackets :close-brackets]
                    :keywd-import #"\$import_(?:sc|scala)"
                    :open-brackets open-brackets
                    :close-brackets close-brackets

                    :state-block [:open-state :close-brackets]
                    :open-state- [:state-prelude :open-brackets]
                    :state-prelude- [:keywd-state :state-args]
                    :keywd-state #"\$state_(?:sc|scala)\s*"
                    :state-args [:identifier :colon :scala-class-name]

                    :method-block [:open-method :close-brackets]
                    :open-method- [:method-prelude :open-brackets]

                    :method-prelude- [:keywd-def :method-name :method-args :method-return-type]
                    :keywd-def #"\$def_(?:sc|scala)\s+"

                    :method-name #"[a-zA-Z_]+[a-zA-Z0-9_]*"

                    :method-args method-args
                    :lparen- lparen
                    :rparen- rparen
                    :arg-type-list- arg-type-list
                    :comma- comma
                    :arg-type- arg-type
                    :colon- colon
                    :identifier #"[a-zA-Z_]+[a-zA-Z0-9_]*"

                    :method-return-type [#"\s*:\s*" :class-name #"\s*"]
                    :class-name class-name
                    :scala-class-name scala-class-name

                    :helper-block [:keywd-helper :open-brackets :close-brackets]
                    :keywd-helper #"\$helpers?_(?:sc|scala)"))))

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
  "Custom parsing logic for the `:common` parser, based on `loose-parse` but pruning the parse tree."
  [file-name]
  (let [parser (parser :common)
        messy-tree (-> (u/pbuffer-from-file parser file-name)
                       p/parse-tree)
        legal-content (->> (:content messy-tree)
                           ;; take the contiguous legal nodes
                           (take-while (fn [node]
                                         (not= (:tag node) :net.cgrand.parsley/unexpected)))
                           ;; Remove Java comments found at beginning of file
                           (remove (fn [node]
                                     (= (:tag node) :java-comment)))
                           ;; JAll doesn't have anything to do with static imports
                           (remove (fn [node]
                                     (and
                                      (= (:tag node) :java-import)
                                      (->> (:content node)
                                           second
                                           (re-find #"^static\s+"))))))]
    (assoc messy-tree :content legal-content)))

(defn common-parse-str
  "Custom parsing logic for the `:common` parser, based on `loose-parse`"
  [input]
  (let [parser (parser :common)
        messy-tree (-> (p/incremental-buffer parser)
                       (p/edit 0 0 input)
                       p/parse-tree)
        legal-content (->> (:content messy-tree)
                           ;; take the contiguous legal nodes
                           (take-while (fn [node]
                                         (not= (:tag node) :net.cgrand.parsley/unexpected)))
                           ;; JAll doesn't have anything to do with static imports
                           (remove (fn [node]
                                     (->> (:content node)
                                          second
                                          (re-find #"^static\s+")))))]
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
        java-package-node (first (filter (partial u/tag= :java-package) content))]
    (-> java-package-node
        :content
        second)))

(defn java-imports
  "Pull out the top-level Java imports in the file"
  [root-node]
  (let [content (:content root-node)]
    (filter (partial u/tag= :java-import) content)))

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
    (filter (partial u/tag= :import-block) contents)))

(defn import-lang
  [import-node]
  (let [contents (:content import-node)
        keywd-node (first (filter (partial u/tag= :keywd-import) contents))]
    (u/lang-legend (string/trim (second
                                 (re-find #"_([^_]+)$" (first (:content keywd-node))))))))

(defn import-body
  [import-node]
  (let [contents (:content import-node)
        ajvm-codes (filter (partial u/tag= :net.cgrand.parsley/unexpected) contents)]
    (apply str (map #(-> % :content first) ajvm-codes))))

(defn blocks-as-imports
  "Create `Import` records out of :import-block blocks"
  [imports]
  (for [import imports]
    (init-import (import-lang import)
                 (import-body import))))

(defn states
  "All `:state-block` nodes from the tree"
  [root-node]
  (let [contents (:content root-node)]
    (filter (partial u/tag= :state-block) contents)))

(defn state-lang
  [state-node]
  (let [contents (:content state-node)
        keywd-node (first (filter (partial u/tag= :keywd-state) contents))]
    (u/lang-legend (string/trim (second
                                 (re-find #"_([^_]+)$" (first (:content keywd-node))))))))

(defn state-name
  [state-node]
  (let [args-node (first (filter (partial u/tag= :state-args) (:content state-node)))
        id-node (first (filter (partial u/tag= :identifier) (:content args-node)))]
    (first (:content id-node))))

(defn state-type
  "For Scala, we have to supply the type as well."
  [state-node]
  (when (= (state-lang state-node) :sc)
    (let [contents (:content state-node)
          state-args-node (first (filter (partial u/tag= :state-args) contents))
          type-node (first
                     (filter (partial u/tag= :scala-class-name)
                             (:content state-args-node)))]
      (first (:content type-node)))))

(defn state-body
  [state-node]
  (let [contents (:content state-node)
        ajvm-codes (filter (partial u/tag= :net.cgrand.parsley/unexpected) contents)]
    (apply str (map #(-> % :content first) ajvm-codes))))

(defn blocks-as-states
  "Create `State` records out of :state-block blocks"
  [states]
  (for [state states]
    (init-state (state-lang state)
                (state-name state)
                (state-type state)
                (state-body state))))

(defn helpers
  "Specialized JAll helper for AJVM languages"
  [root-node]
  (let [contents (:content root-node)]
    (filter (partial u/tag= :helper-block) contents)))

(defn helper-lang
  [helper-node]
  (let [contents (:content helper-node)
        keywd-node (first (filter (partial u/tag= :keywd-helper) contents))]
    (u/lang-legend (string/trim (second
                                 (re-find #"_([^_]+)$" (first (:content keywd-node))))))))

(defn helper-body
  [helper-node]
  (let [contents (:content helper-node)
        ajvm-codes (filter (partial u/tag= :net.cgrand.parsley/unexpected) contents)]
    (apply str (map #(-> % :content first) ajvm-codes))))

(defn blocks-as-helpers
  "Create `Helper` records out of :helper-block blocks"
  [helpers]
  (for [helper helpers]
    (init-helper (helper-lang helper)
                 (helper-body helper))))

(defn methods
  "Given root parse-tree node, return all code blocks"
  [root-node]
  (let [contents (:content root-node)
        method-blocks (filter (partial u/tag= :method-block) contents)]
    (remove (fn [block]
              (= (:tag (second (:content block)))
                 :net.cgrand.parsley/unexpected))
            method-blocks)))

(defn method-lang
  "Return keyword for language code of given code block, e.g., `:clj`

   In order to make the parser unambiguous, we have to hard-code a language-specific path at some point. This is that point, which is why the below code looks ugly."
  [node]
  (let [content (:content node)
        keywd-node (first (filter (partial u/tag= :keywd-def) content))]
    (u/lang-legend (string/trim (second
                                 (re-find #"_([^_]+)$" (first (:content keywd-node))))))))

(defn method-name
  "Return name of method for code block"
  [node]
  (let [content (:content node)
        method-name-node (first (filter (partial u/tag= :method-name) content))]
    (-> method-name-node :content first)))

(defn method-args
  "Get the args for the code block's method"
  [node]
  (let [content (:content node)
        method-args-node (first (filter (partial u/tag= :method-args) content))]
    (apply sorted-map (map #(-> % :content first) (u/clean-syntactic-cruft method-args-node)))))

(defn method-return-type
  "Get the return type for the block method"
  [node]
  (let [lang (method-lang node)
        content (:content node)
        return-type-node (first (filter (partial u/tag= :method-return-type) content))
        return-type (-> (u/clean-syntactic-cruft return-type-node)
                        first
                        :content
                        first)]
    return-type))

(defn method-body
  "Extract all the code inside the method def for a given code block, which we keep track of by not parsing it at all :-)"
  [node]
  {:pre [(= (:tag (last (:content node))) :close-brackets)]}
  (let [content (:content node)
        ajvm-codes (filter (partial u/tag= :net.cgrand.parsley/unexpected) content)]
    (apply str (map #(-> % :content first) ajvm-codes))))

(defn blocks-as-methods
  "Given all code block nodes from a given parse tree, create the appropriate `Method` records."
  [blocks]
  (for [block blocks]
    (init-method (method-lang block)
                 (method-name block)
                 (method-args block)
                 (method-return-type block)
                 (method-body block))))