;;   Copyright (c) Daniel Gregoire. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns com.semperos.jall.generate.clojure
  (:use [clojure.pprint :only [pprint]])
  (:require [com.semperos.jall.util :as u]
            [clojure.string :as string]))

(def clj-method-prefix "java-")
(def clj-ctor-name "init")

(defn clj-method-sigs
  "The method signatures in :gen-class"
  [methods]
  (vec ;; because I like everything in vectors
   (for [{:keys [name args return-type]} methods]
     (let [genericless-args (into {} (for [[k v] args]
                                       [k (second (re-find #"([^<]+)<?" v))]))]
       [(symbol (u/translate-method-name :clj name)) (vec (map symbol (vals genericless-args))) (symbol return-type)]))))

(defn clj-gen-class
  "The class name is set explicitly to `full-class-name` plus a language-specific suffix."
  [full-class-name state methods]
  (list :gen-class
        :name (symbol (u/translate-class-name :clj full-class-name))
        :init (symbol clj-ctor-name)
        :constructors {[] []}
        :state (symbol (get state :name "state"))
        :implements [(symbol (u/translate-interface-name :clj full-class-name))]
        :prefix clj-method-prefix))

(defn clj-ns
  [full-class-name import state methods]
  (let [klass (symbol (u/translate-class-name :clj full-class-name))
        import-body (if (:body import)
                      (read-string (:body import))
                      nil)]
    (remove nil?
     (list 'ns klass
           import-body
           (clj-gen-class full-class-name state methods)))))

(defn clj-ctor
  "Define a constructor for our Java-facing class defined by `:gen-class`"
  [state]
  (let [body (if (:body state)
               (read-string (:body state))
               nil)]
    (list 'defn
          (symbol (str clj-method-prefix clj-ctor-name))
          []
          [[] body])))

(defn clj-helpers
  "A helper is a JAll block of possibly multiple function definitions.

   Since `read-string` will only read in the first sexpr it finds, we wrap the input in an extra set of parentheses. For this reason, we `concat` this input."
  [helper-forms]
  (let [helpers (str "(" (:body helper-forms) ")")]
    (read-string helpers)))

(defn clj-defn
  "Generate a `defn` form, again wrapping the body of code in parens and `concat`ing it to make `read-string` read everything in."
  [{:keys [name args body]}]
  (let [wrapped-body (str "(" body ")")
        body-code (read-string wrapped-body)
        starting-defn (list 'defn
                            (symbol (str clj-method-prefix (u/translate-method-name :clj name)))
                            (vec (map symbol (concat ['this] (keys args)))))]
    (concat starting-defn body-code)))

(defn clj-file
  [full-class-name import state helpers methods]
  (let [starting-code [(clj-ns full-class-name import state methods)
                       (clj-ctor state)]
        code-with-helpers (reduce (fn [code helper-forms]
                                     (concat code (clj-helpers helper-forms)))
                                   starting-code
                                   helpers)]
    (reduce (fn [state method]
              (concat state [(clj-defn method)])) code-with-helpers methods)))

(defn output-file
  "Output a string of the to-be file contents of the given Clojure source file."
  [full-class-name import state helpers methods]
  (let [pieces (clj-file full-class-name import state helpers methods)]
    (string/join "\n"
                 (for [piece pieces]
                   (str (with-out-str (pprint piece)))))))

