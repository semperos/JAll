(ns jall.generate.jruby
  (:use [clojure.pprint :only [pprint]])
  (:require [jall.util :as u]
            [clojure.string :as string]))

(defn rb-prelude
  "Things at top of file"
  [import]
  (let [starting-code ["# encoding: utf-8"
                       "require 'java'\n"]]
    (if import
      (conj starting-code import)
      starting-code)))

(defn rb-class-start
  [full-class-name methods]
  (let [package (u/package-from-class-name full-class-name)
        klass (str (u/class-from-class-name full-class-name) "Rb")]
    [(format "java_package '%s'" package)
     (format "class %s\n" klass)]))

(defn rb-class-methods
  [methods]
  (flatten
   (for [{:keys [name args body return-type]} methods]
     (let [args-str (apply str (interpose "," (keys args)))]
       [(format "  java_signature '%s %s(%s)'"
                return-type
                (u/clj-to-java name)
                (u/java-method-args-str args))
        (format "  def %s(%s)" name args-str)
        (format "    %s" (string/trim body))
        "  end\n"]))))

(defn rb-class-end
  []
  ["end\n"])

(defn rb-file
  [full-class-name import methods]
  (flatten
   (concat (rb-prelude import)
           (rb-class-start full-class-name methods)
           (rb-class-methods methods)
           (rb-class-end))))

(defn output-file
  [full-class-name import methods]
  (let [pieces (rb-file full-class-name import methods)]
    (string/join "\n"
                 (for [piece pieces]
                   (str (with-out-str (print piece)))))))