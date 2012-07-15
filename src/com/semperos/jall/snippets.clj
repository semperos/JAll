;;   Copyright (c) Daniel Gregoire. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; Commented-out code we don't yet want to lose
(ns com.semperos.jall.snippets)

                ;; :clj-value #{:clj-number :clj-string}
                ;; :clj-number #"[0-9]+"
                ;; :clj-string #"\".*?[^\\]\""
                ;; :equals #"\s+=\s+"
                ;; :identifier #{:java-id :clj-id :jruby-id :scala-id}
                ;; :value #{:java-value :clj-value :jruby-value :scala-value}
                ;; :java-id #"[a-z]+[a-zA-Z0-9_]*"
                ;; :scala-id #"[a-z]+[a-zA-Z0-9_]*"
                ;; :clj-id #"[a-zA-Z-]+"
                ;; :jruby-id #"[a-z]+[a-zA-Z0-9_]*"
                ;; :java-value #{:java-number :java-string :java-object}
                ;; :scala-value #{:scala-number :scala-string :scala-symbol :scala-object}
                ;; :clj-value #{:clj-number :clj-string :clj-regex :clj-symbol :clj-keyword :clj-list :clj-map :clj-set}
;; :jruby-value #{:jruby-number :jruby-string :jruby-regex :jruby-symbol :jruby-array :jruby-hash}

                ;; Unless we write a whole Java parser with JAll semantics built-in,
                ;; the `:modifiers` rule matches everywhere and results in "unfinished"
                ;; entries in the parse tree which shadow the `:code-block` rules
                ;; defined for JAll
                ;;
                ;; An acceptable alternative, for now, will be to pull the Java class
                ;; name from the file being read.
                ;;
                ;; :java-class [:modifiers* :keywd-class :java-class-id :lbrace]
                ;; :modifiers #{"public" "protected" "private" "static" "abstract" "final" "native" "synchronized" "transient" "volatile" "strictfp"}
                ;; :keywd-class #"\s+class\s+"
;; :java-class-id #"[A-Z]+[a-zA-Z0-9_]*"
;; :clj-symbol #"[a-zA-Z-\?!]+(?:(?!,\s*))*"

;; :lbrace "{"

;; (defn code-java-class-name
;;   "Pull out the name of the Java class we're dealing with."
;;   [root-node]
;;   (let [content (:content root-node)
;;         java-class-node (first (filter #(= (:tag %) :java-class) content))]
;;     (:content java-class-node)))

;; How to use Parsley
;; (def myp (p/parser {:main :expr*}
;;                    :expr #{"x" ["(" :expr* ")"]}))
;; (def mys-begin "(x(x")
;; (def mys-end "))")
;; (def mybuf (p/incremental-buffer myp))
;; (def mybuf-unfinished (p/edit mybuf 0 0 mys-begin))
;; (def mybuf-complete (u/append-to-buffer mybuf-unfinished mys-end))
;; (def mytree (myp (str mys-begin mys-end)))


;; (fs/copy working-pom (clojure.string/join "/" [project-root "pom.xml"]))
  ;; let working-pom (->> "resources/jall_pom.xml"
  ;;                      (.getResource (clojure.lang.RT/baseLoader))
  ;;                      io/file
  ;;                      str)