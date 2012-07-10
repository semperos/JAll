(ns jall.generate.java-support
  (:require [clojure.string :as string]
            [jall.util :as u]))

(defn java-package
  [full-class-name]
  (let [package (second (re-find #"(.*?)\.[^\.]+$" full-class-name))]
    (format "package %s;\n" package)))

(defn java-interface-decl
  [full-class-name lang]
  (let [interface-name (last (string/split (u/translate-interface-name lang full-class-name) #"\."))]
    (format "public interface %s {" interface-name)))

(defn java-method-args-str
  [method-args]
  (apply str (flatten
              (interpose ", "
                         (for [[k v] method-args]
                           ;; key is the argument name
                           ;; value is the type of the arg
                           [v " " (u/clj-to-java k)])))))

(defn java-interface-methods
  [methods]
  (for [{:keys [name args return-type]} methods]
    (format "%s %s(%s);" return-type (u/clj-to-java name) (java-method-args-str args))))

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
