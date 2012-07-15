;;   Copyright (c) Daniel Gregoire. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns com.semperos.jall.generate.scala
  (:use [clojure.pprint :only [pprint]])
  (:require [com.semperos.jall.util :as u]
            [clojure.string :as string]))

(defn sc-package
  "Package name for this Scala file"
  [full-class-name]
  [(str "package " (u/package-from-class-name full-class-name))])

(defn sc-prelude
  "Things at top of file"
  [import]
  (if import
    [(string/trim (:body import))
     ""]
    [""]))

(defn sc-class-start
  [full-class-name]
  (let [package (u/package-from-class-name full-class-name)
        klass (str (u/class-from-class-name full-class-name) "Sc")]
    [(format "class %s {\n" klass)]))

(defn sc-class-helpers
  "Helpers represent non-Java-facing methods that can be used by official JAll methods. They can really be anything; they are simply Ruby source inserted at the top of the class definition before the regular JAll methods are added."
  [helpers]
  (flatten
   (for [{:keys [body]} helpers]
     [body
      "\n"])))

(defn sc-class-methods
  [methods]
  (flatten
   (for [{:keys [name args body return-type]} methods]
     (let [args-str (apply str
                     (interpose ","
                                (for [[arg arg-type] args]
                                  (str arg " : " arg-type))))]
       (if (= return-type "void")
         [(format "  def %s(%s) {" name args-str return-type)
          (format "    %s" (string/trim body))
          "  }\n"]
         [(format "  def %s(%s) : %s = {" name args-str return-type)
          (format "    %s" (string/trim body))
          "  }\n"])))))

(defn sc-class-end
  []
  ["}\n"])

(defn sc-file
  [full-class-name import helpers methods]
  (flatten
   (concat (sc-package full-class-name)
           (sc-prelude import)
           (sc-class-start full-class-name)
           (sc-class-helpers helpers)
           (sc-class-methods methods)
           (sc-class-end))))

(defn output-file
  [full-class-name import helpers methods]
  (let [pieces (sc-file full-class-name import helpers methods)]
    (string/join "\n"
                 (for [piece pieces]
                   (str (with-out-str (print piece)))))))