;;   Copyright (c) Daniel Gregoire. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; Functionality for generating AJVM code
;;
;; Coerce values each place they're needed.
;;
(ns com.semperos.jall.generate
  (:use [clojure.pprint :only [pprint]])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [fs.core :as fs]
            [com.semperos.jall.util :as u]
            [com.semperos.jall.generate.clojure :as clj]
            [com.semperos.jall.generate.ruby :as rb]
            [com.semperos.jall.generate.scala :as sc]
            [com.semperos.jall.generate.java :as j]
            [com.semperos.jall.generate.java-support :as support])
  (:import [java.util.regex Matcher Pattern]))

(defrecord File [lang class-name content support-file?])

(defn init-file
  "Initialize a new `File` record"
  ([] (init-file nil nil nil nil))
  ([lang] (init-file lang nil nil nil))
  ([lang class-name] (init-file lang class-name nil nil))
  ([lang class-name content] (init-file lang class-name content nil))
  ([lang class-name content support-file?] (File. lang class-name content support-file?)))

(defn file?
  "True if the type of argument is a `File`"
  [f]
  (= (type f) File))

(defn output-file
  "Intermediate fn for output-ajvm-files for dispatching on value of lang"
  [full-class-name lang imports states helpers methods]
  (let [right-import (first (filter (partial u/lang= lang) imports))
        right-state (first (filter (partial u/lang= lang) states))
        right-helpers (filter (partial u/lang= lang) helpers)
        right-methods (filter (partial u/lang= lang) methods)]
    ;; TODO: opportunity for multi-method here
    (case lang
      :clj (init-file :clj
                      full-class-name
                      (clj/output-file full-class-name right-import right-state right-helpers right-methods))
      :rb (init-file :rb
                     full-class-name
                     (rb/output-file full-class-name right-import right-state right-helpers right-methods))
      :sc (init-file :sc
                     full-class-name
                     (sc/output-file full-class-name right-import right-state right-helpers right-methods)))))

(defn output-ajvm-files
  "Given all the method definitions found in the JAll source document, call the appropriate function for transforming method def's for each language"
  [full-class-name imports states helpers methods]
  ;; all-langs based on method definitions, so unused imports aren't included per-language
  (let [all-langs (reduce (fn [state item]
                            (conj state (:lang item)))
                          #{}
                          methods)]
    (for [lang all-langs]
      (output-file full-class-name lang imports states helpers methods))))

(defn output-support-for-clj-file
  [full-class-name methods]
  (let [pieces (flatten (support/support-file-for-lang full-class-name :clj methods))]
    (string/join "\n" pieces)))

(defn output-java-support-file
  "Intermediate fn for output-java-support-files for dispatching on value of lang"
  [full-class-name lang methods]
  (let [right-methods (filter (fn [item] (= (:lang item) lang)) methods)]
    (case lang
      :clj (init-file :java
                      (u/translate-interface-name full-class-name :clj)
                      (output-support-for-clj-file full-class-name right-methods)
                      true ;; support-interface?
                      ))))

(defn output-java-support-files
  [full-class-name methods]
  (let [;; all-langs (reduce (fn [state item]
        ;;                     (conj state (:lang item)))
        ;;                   #{}
        ;;                   methods)
        ;;
        ;; Only Clojure seems to *need* a Java Interface to get generics right
        all-langs [:clj] ]
    (for [lang all-langs]
      (output-java-support-file full-class-name lang methods))))

;; ### Java equivalents for generated Clojure classes/functions ###

;; TODO: Another example of where the file should be parsed line-by-line,
;; not slurped in. This is part of the whole point of using Parsley for parsing.
(defn output-java-file
  [full-class-name langs jall-source-file]
  (let [file-content (slurp jall-source-file)]
    (init-file :java
               full-class-name
               (j/transform-java-source file-content full-class-name langs))))