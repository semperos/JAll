;;   Copyright (c) Daniel Gregoire. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns com.semperos.jall.generate.java-support
  (:require [clojure.string :as string]
            [com.semperos.jall.util :as u]))

(defn java-package
  [full-class-name]
  (let [package (second (re-find #"(.*?)\.[^\.]+$" full-class-name))]
    (format "package %s;\n" package)))

(defn java-interface-decl
  [full-class-name lang]
  (let [interface-name (last (string/split (u/translate-interface-name full-class-name lang) #"\."))]
    (format "public interface %s {" interface-name)))

(defn java-interface-methods
  [methods]
  (for [{:keys [name args return-type]} methods]
    (format "%s %s(%s);" return-type (u/clj-to-java name) (u/java-method-args-str args))))

(defn java-interface-close
  []
  "}")

(defn java-interface
  [full-class-name lang methods]
  (list (java-interface-decl full-class-name lang)
        (java-interface-methods methods)
        (java-interface-close)))

(defn support-file-for-lang
  "Support = Java Interface, done per-lang for tighter requirements"
  [full-class-name lang methods]
  (list (java-package full-class-name)
        (java-interface full-class-name lang methods)))

