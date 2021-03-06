;;   Copyright (c) Daniel Gregoire. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns com.semperos.jall.util
  (:require [net.cgrand.parsley :as p]
            [net.cgrand.parsley.tree :as t]
            [fs.core :as fs]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(defn buffer-length
  "Get the length of the string that the given `buffer` is dealing with."
  [buffer]
  {:pre [(map? buffer)
         (contains? buffer :buffer)]}
  (t/len (:buffer buffer)))

(defn append-to-buffer
  "Append new `content` (e.g., a new line) to the given `buffer`, returning the new buffer created."
  [buffer ^String content]
  (let [offset (buffer-length buffer)]
    (p/edit buffer offset 0 content)))

;; TODO: Parsing should be done incrementally (line-by-line),
;; not via `slurp`ing the file. This is part of the whole reason
;; for using Parsley to parse.
(defn pbuffer-from-file
  "Given something file-able (see clojure.java.io/reader), return a *P*arsley *buffer* with the contents of that file and the `jall` parser."
  [parser f]
  {:pre [(.exists (io/as-file f))]}
  (let [input (slurp f)]
    (-> (p/incremental-buffer parser)
        (p/edit 0 0 input))))

(defn remove-top-unexpected
  "Remove all unexpected tags from `:content` of root node in parse tree. This essentially white-lists what the parser knows from the rest of the file's top-level content."
  [root-node]
  (let [nodes (:content root-node)]
    (update-in root-node [:content] (fn [original-contents]
                                    (remove (fn [node]
                                              (= (:tag node) :net.cgrand.parsley/unexpected))
                                            original-contents)))))

(defn tag=
  "Function for easily filter by nodes with tags of a certain value"
  [expected-tag actual-node]
  (= (:tag actual-node) expected-tag))

(defn lang=
  "Function for easily filter by nodes with `:lang` of a certain value"
  [expected-lang actual-node]
  (= (:lang actual-node) expected-lang))

(defn class-name-from-file
  [file-name]
  (fs/name file-name))

(defn dashes-to-camel-case
  [s]
  (reduce (fn [state item]
             (.replaceAll state item (string/upper-case (apply str (rest item)))))
           s
           (re-seq #"-[^-]" s)))

(defn snake-case-to-camel-case
  [s]
  (reduce (fn [state item]
            (.replaceAll state item (string/upper-case (apply str (rest item)))))
          s
          (re-seq #"_[^_]" s)))

(defn clj-to-java
  "Dashes to CamelCase, illegal things replaced"
  [s]
  (-> s
      dashes-to-camel-case
      (.replaceAll "_BANG" "!")
      (.replaceAll "_QMARK" "?")
      (.replaceAll "_STAR" "*")))

(defn java-method-args-str
  [method-args]
  (apply str (flatten
              (interpose ", "
                         (for [[k v] method-args]
                           ;; key is the argument name
                           ;; value is the type of the arg
                           [v " " (clj-to-java k)])))))

(defn package-from-class-name
  [full-class-name]
  (let [parts (string/split full-class-name #"\.")]
    (string/join "." (butlast parts))))

(defn class-from-class-name
  [full-class-name]
  (let [parts (string/split full-class-name #"\.")]
    (last parts)))

(defn translate-class-name
  "AJVM classes have a suffix appended to make things easier to recognize in generated Java code."
  [full-class-name lang]
  (case (keyword lang)
    :clj (str full-class-name "Clj")
    :rb  (str full-class-name "Rb")
    :sc  (str full-class-name "Sc")
    full-class-name))

(defn translate-interface-name
  "AJVM classes implement an auto-generated Java interface with the same methods to ensure typing."
  [full-class-name lang]
  (let [full-name (translate-class-name full-class-name lang)
        parts (string/split full-name #"\.")
        package-portion (butlast parts)
        class-portion (last parts)
        interface-name (str "I" class-portion)]
    (string/join "." (concat package-portion [interface-name]))))

(defn translate-method-name
  "Given a target language and method name, generate the Java class/method that would result"
  [method-name lang]
  (case (keyword lang)
    :clj (dashes-to-camel-case method-name)
    :rb (snake-case-to-camel-case method-name)
    method-name))

(defn clean-syntactic-cruft
  "Remove things from the content of a Parsley node that are not maps (i.e. matched only a regex or string in a parser, and not a rule as a whole)"
  [node]
  (let [args (:content node)]
    (filter (fn [arg] (map? arg)) args)))

(defn lang-legend
  "Return canonical (internal) name for a given language, defaulting to `:java`"
  [s]
  (case (keyword s)
    :clj :clj
    :clojure :clj
    :rb :rb
    :ruby :rb
    :jruby :rb
    :sc :sc
    :scala :sc
    :java))

(defn file-langs
  "Return the languages found in all `File` records"
  [file-records]
  (map #(:lang %) file-records))