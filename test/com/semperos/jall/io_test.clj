(ns com.semperos.jall.io-test
  (:use expectations.scenarios
        com.semperos.jall.io)
  (:require [com.semperos.jall.generate :as gen]))

(scenario
 (given [lang folder-name] (expect (src-lang-folder lang) folder-name)
   (expect
    :clj "clojure"
    :rb "ruby"
    :sc "scala"
    :java "java"
    :foo "java")))

(scenario
 (given [lang lang-ext] (expect (lang-extension lang) lang-ext)
   (expect
    :clj ".clj"
    :rb ".rb"
    :sc ".scala"
    :java ".java"
    :foo ".java")))

(scenario
 (given [lang file-name] (expect (gen/init-file lang "com.example.Sample") file-name)
   (expect
    :clj "SampleClj.clj"
    :rb "SampleRb.rb"
    :sc "SampleSc.scala")))

(scenario
 ;; Necessary dir's for non-support files (regular)
 (given (necessary-src-dirs (gen/init-file :clj "com.example.Sample" "foo" false))
   (expect
    identity coll?
    count #(> % 0)
    identity #(every? string? %)
    identity ["target" "src" "main" "clojure" "com" "example"])))

(scenario
 ;; Necessary dir's for support files
 (given (necessary-src-dirs (gen/init-file :clj "com.example.Sample" "foo" true))
   (expect
    identity coll?
    count #(> % 0)
    identity #(every? string? %)
    identity ["target" "src" "support" "clojure" "com" "example"])))