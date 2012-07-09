;; Crutches while I learn Parsley
(ns jall.util
  (:require [net.cgrand.parsley :as p]
            [net.cgrand.parsley.tree :as t]
            [fs.core :as fs]
            [clojure.java.io :as io]))

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

(defn class-name-from-file
  [file-name]
  (fs/name file-name))

(defn dashes-to-camel-case
  [s]
  (reduce (fn [state item]
             (.replaceAll state item (.toUpperCase (apply str (rest item)))))
           s
           (re-seq #"-[^-]" s)))

(defn clj-to-java
  "Dashes to CamelCase, illegal things replaced"
  [s]
  (-> s
      dashes-to-camel-case
      (.replaceAll "_BANG" "!")
      (.replaceAll "_QMARK" "?")
      (.replaceAll "_STAR" "*")))

(defn clean-syntactic-cruft
  "Remove things from the content of a Parsley node that are not maps (i.e. matched only a regex or string in a parser, and not a rule as a whole)"
  [node]
  (let [args (:content node)]
    (filter (fn [arg] (map? arg)) args)))