;;   Copyright (c) Daniel Gregoire. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns com.semperos.jall.generate.java
  (:require [com.semperos.jall.util :as u]
            [com.semperos.jall.parser :as p]
            [clojure.string :as string]))

(defn re-positions [re s]
  "Return positions for all matches found for `re` against `s`, including start and end.

Credits to Brian Carper for initial code: http://stackoverflow.com/questions/3262195/compact-clojure-code-for-regular-expression-matches-and-their-position-in-string"
        (loop [m (re-matcher re s)
               res []]
          (if (.find m)
            (recur m (conj res [(.start m) (.end m)]))
            res)))

(defn comment-out-positions
  "Return a seq of pairs, representing the starting and ending index (exclusive) that should be commented out.

   This is another instance where the parser should be used, with buffer editing, not adhoc regexes."
  [file-content]
  (let [import-matches  (re-positions #"(?ms)\$import.*?^\s*\}\}"    file-content)
        state-matches   (re-positions #"(?ms)\$state.*?^\}\}"        file-content)
        helper-matches  (re-positions #"(?ms)\$helpers?.*?^\s*\}\}"  file-content)
        def-matches     (re-positions #"(?ms)\$def.*?^\s*\}\}"       file-content)]
    (concat import-matches state-matches helper-matches def-matches)
    ;; (for [match matches]
    ;;   (let [idx-match (.indexOf file-content match)]
    ;;     [idx-match
    ;;      ;; Plus 2 to account for the two brackets
    ;;      (+ 2 (.indexOf file-content "}}" idx-match))]))
    ))

(defn comment-out-regions
  "Given a seq of pairs with begin/end indices for commenting out in a Java file, return the final, concatenated strings with those regions commented out."
  [file-content positions]
  (:output (reduce (fn [state [start end]]
                     (let [old-output (:output state)
                           old-delta  (:delta state)
                           real-start  (+ old-delta start)
                           real-end    (+ old-delta end)
                           initial-sub (.substring old-output 0 real-start)
                           inner-sub   (.substring old-output real-start real-end)
                           post-sub    (.substring old-output real-end)
                           start-comment "/*\n"
                           end-comment "\n*/"
                           this-delta (count (str start-comment end-comment))]
                       {:output (str initial-sub start-comment inner-sub end-comment post-sub)
                        :delta (+ old-delta this-delta)}))
                   {:output file-content :delta 0}
                   positions)))

(defn comment-out-method-definitions
  [file-content]
  (let [positions (comment-out-positions file-content)]
    (comment-out-regions file-content positions)))

(defn transform-method-call
  [state to-replace lang class-name method-name]
  (.replaceAll state to-replace
               (str
                "new "
                (u/translate-class-name lang class-name)
                "()."
                (u/translate-method-name lang method-name)
                "(")))

(defn replace-method-calls
  "Find calls like `$clj_my-method-here(foo)` and replace them with the correct Java.

   TODO: This should probably be handled using things we actually parse out, so we're not parsing all over the place in different ways."
  [file-content full-class-name]
  (let [parse-tree (p/adhoc-parse (p/parser :common) file-content)
        java-package (p/java-package parse-tree)
        java-imports (map p/java-import-class
                          (p/java-imports parse-tree)) 
        method-sigs (re-seq #"(?s)\$(clj|rb|sc)_([^\(]+)\(" file-content)]
    (reduce (fn [state [whole lang method-name]]
              (let [to-replace (str "\\" ;; because $ is a regex meta-character in need of escape
                                    (apply str (butlast whole)) ;; because matching for `whole` above is easier when including the opening `(`
                                    "\\(")]
                (if (.contains method-name "/")
                  ;; if there's a slash, it means this is a fully-qualified reference to another class
                  (let [other-class (second (re-find #"([^/]+)/" method-name))
                        real-method-name (second (re-find #"/(.*)$" method-name))]
                    (if (.contains other-class ".")
                      ;; if other-class has a dot, then it's fully-qualified, leave it alone
                      (transform-method-call state to-replace lang other-class real-method-name)
                      ;; else it's not fully-qualified, so reference the java-imports to make it so
                      (if (some #{other-class} (map u/class-from-class-name java-imports))
                        ;; if an explicit import has the same class name, use it (first that matches)
                        (transform-method-call state
                                               to-replace
                                               lang
                                               (first
                                                (filter (fn [impt]
                                                          (= (u/class-from-class-name impt)
                                                             other-class)) java-imports))
                                               real-method-name)
                        ;; else use the package of the file we're currently in + the other-class value
                        (transform-method-call state
                                               to-replace
                                               lang
                                               (str java-package "." other-class)
                                               real-method-name))))
                  (transform-method-call state to-replace lang full-class-name method-name))))
            file-content
            method-sigs)))

(defn add-ajvm-imports
  "Given the full class name of the current file and all AJVM languages used, add the necessary import statements to the final Java file"
  [content full-class-name langs]
  (let [imports (reduce (fn [state item]
                          (conj state (str "import " (u/translate-class-name item full-class-name) ";")))
                        []
                        langs)
        imports-str (string/join "\n" imports)]
    (.replaceFirst content "(?s)\n\\s*import" (str "\n\n" imports-str "\n\nimport"))))

