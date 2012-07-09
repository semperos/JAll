;; Functionality for generating AJVM code
;;
;; Coerce values each place they're needed.
;;
(ns jall.generate
  (:use [clojure.pprint :only [pprint]])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [fs.core :as fs]
            [jall.util :as u]
            [jall.generate.clojure :as clj]
            [jall.generate.java :as j]
            [jall.generate.java-support :as support])
  (:import [java.util.regex Matcher Pattern]))

(defrecord File [lang class-name content support-file?])

(defn init-file
  "Initialize a new `File` record"
  ([] (init-file nil nil nil nil))
  ([lang] (init-file lang nil nil nil))
  ([lang class-name] (init-file lang class-name nil nil))
  ([lang class-name content] (init-file lang class-name content nil))
  ([lang class-name content support-file?] (File. lang class-name content support-file?)))

(defn output-file
  "Intermediate fn for output-ajvm-files for dispatching on value of lang"
  [full-class-name lang imports methods]
  (println "OUTPUT FILE LANG")
  (println lang)
  (println "IMPORTS")
  (println imports)
  (println "IMPORTS FOR LANG")
  (println (filter (fn [item] (= (:lang item) lang)) imports))
  (let [right-methods (filter (fn [item] (= (:lang item) lang)) methods)
        right-imports (filter (fn [item] (= (:lang item) lang)) imports)]
    (case lang
      :clj (init-file :clj
                      full-class-name
                      ;; there should only be on !import statement per language per file
                      (clj/output-file full-class-name (first right-imports) right-methods)))))

(defn output-ajvm-files
  "Given all the method definitions found in the JAll source document, call the appropriate function for transforming method def's for each language"
  [full-class-name imports methods]
  ;; all-langs based on method definitions, so unused imports aren't included per-language
  (let [all-langs (reduce (fn [state item]
                            (conj state (:lang item)))
                          #{}
                          methods)]
    (for [lang all-langs]
      (output-file full-class-name lang imports methods))))

(defn output-support-for-clj-file
  [full-class-name methods]
  (let [pieces (flatten (support/support-file-for-lang full-class-name :clj methods))]
    (string/join "\n" pieces)))

(defn output-java-support-file
  "Intermediate fn for output-java-support-files for dispatching on value of lang"
  [full-class-name lang methods]
  (let [right-methods (filter (fn [state item] (= (:lang item) lang)) methods)]
    (case lang
      :clj (init-file :java
                      (u/translate-interface-name :clj full-class-name)
                      (output-support-for-clj-file full-class-name methods)
                      true ;; support-interface?
                      ))))

(defn output-java-support-files
  [full-class-name methods]
  (let [all-langs (reduce (fn [state item]
                            (conj state (:lang item)))
                          #{}
                          methods)]
    (for [lang all-langs]
      (output-java-support-file full-class-name lang methods))))

;; ### Java equivalents for generated Clojure classes/functions ###

(defn output-java-file
  [full-class-name langs jall-source-file]
  (let [content (slurp jall-source-file)]
    (init-file :java
               full-class-name
               (-> content
                   (j/comment-out-method-definitions)
                   (j/replace-method-calls full-class-name)
                   (j/add-ajvm-imports full-class-name langs)))))