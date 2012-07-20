;;   Copyright (c) Daniel Gregoire. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns com.semperos.jall.generate.ruby
  (:use [clojure.pprint :only [pprint]])
  (:require [com.semperos.jall.util :as u]
            [clojure.string :as string]))

(defn rb-prelude
  "Things at top of file"
  [import]
  (let [starting-code ["# encoding: utf-8"
                       "require 'rubygems'" ;; consider Bundler at some point
                       "require 'java'\n"]]
    (if import
      (-> starting-code
          (conj (string/trim (:body import)))
          (conj "\n"))
      starting-code)))

(defn rb-class-start
  [full-class-name state]
  (let [package (u/package-from-class-name full-class-name)
        klass (str (u/class-from-class-name full-class-name) "Rb")
        state-name (if (:name state)
                     (format "  attr_accessor :%s\n" (:name state))
                     nil)]
    [(format "java_package '%s'" package)
     (format "class %s" klass)
     state-name]))

(defn rb-class-ctor
  [state]
  (if (:body state)
    ;; default ctor with state handling
    (let [name (:name state)
          value (string/trim (:body state))]
      ["  def initialize"
       (format "    @%s = %s" name value)
       "  end"])
    ;; for now, just an empty ctor
    ["  def initialize"
     "  end"]))

(defn rb-class-helpers
  "Helpers represent non-Java-facing methods that can be used by official JAll methods. They can really be anything; they are simply Ruby source inserted at the top of the class definition before the regular JAll methods are added."
  [helpers]
  (flatten
   (for [{:keys [body]} helpers]
     [body
      "\n"])))

(defn rb-class-methods
  [methods]
  (flatten
   (for [{:keys [name args body return-type]} methods]
     (let [args-str (apply str (interpose "," (keys args)))]
       [(format "  java_signature '%s %s(%s)'"
                return-type
                (u/snake-case-to-camel-case name)
                (u/java-method-args-str args))
        (format "  def %s(%s)" (u/snake-case-to-camel-case name) args-str)
        (format "    %s" (string/trim body))
        "  end\n"]))))

(defn rb-class-end
  []
  ["end\n"])

(defn rb-file
  [full-class-name import state helpers methods]
  (remove nil?
          (flatten
           (concat (rb-prelude import)
                   (rb-class-start full-class-name state)
                   (rb-class-ctor state)
                   (rb-class-helpers helpers)
                   (rb-class-methods methods)
                   (rb-class-end)))))

(defn output-file
  [full-class-name import state helpers methods]
  (let [pieces (rb-file full-class-name import state helpers methods)]
    (string/join "\n"
                 (for [piece pieces]
                   (str (with-out-str (print piece)))))))