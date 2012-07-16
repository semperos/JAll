(ns com.semperos.jall.compiler-test
  (:use expectations.scenarios
        com.semperos.jall.compiler
        [com.semperos.jall.test-utils :only [sample-src sample-src-dir]])
  (:require [com.semperos.jall.parser :as parser]
            [com.semperos.jall.util :as u]
            [com.semperos.jall.generate :as gen]
            [net.cgrand.parsley :as parsley]))

(scenario
 ;; Basic success of `jall-parse-tree`
 (expect net.cgrand.parsley.Node (jall-parse-tree sample-src))
 (expect :root (:tag (jall-parse-tree sample-src)))
 (expect empty?
         (filter (fn [node]
                   (= (:tag node) :net.cgrand.parsley/unexpected))
                 (jall-parse-tree sample-src))))

(scenario
 (given (find-source-files sample-src-dir)
   (expect
    count 1
    first java.io.File
    #(str (first %)) sample-src)))

(scenario
 (given (produce-ajvm-files sample-src (jall-parse-tree sample-src))
   (expect
    identity coll?
    identity (fn [coll]
               (every? #(= com.semperos.jall.generate.File (type %)) coll))
    identity (fn [coll]
               (some #(= :clj (:lang %)) coll))
    identity (fn [coll]
               (some #(= :rb (:lang %)) coll))
    identity (fn [coll]
               (some #(= :sc (:lang %)) coll)))))

(scenario
 (given (produce-java-support-files sample-src
                                    (jall-parse-tree sample-src)
                                    (u/file-langs (produce-ajvm-files sample-src (jall-parse-tree sample-src))))
   (expect
    identity coll?
    identity (fn [coll]
               (every? #(= com.semperos.jall.generate.File (type %)) coll)))))

