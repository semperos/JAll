(ns jall.parser
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

;; Main parser
(defn parser
  "Wrap parser in a fn"
  []
  (let [jall-parser (p/parser {:main :expr*
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