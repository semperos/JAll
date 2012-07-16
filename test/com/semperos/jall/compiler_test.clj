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
 ;; Finding JAll source files
 (given (find-source-files sample-src-dir)
   (expect
    count 1
    first java.io.File
    #(str (first %)) sample-src)))

(scenario
 ;; Producing AJVM files from source
 (given (produce-ajvm-files sample-src (jall-parse-tree sample-src))
   (expect
    identity coll?
    identity             #(every? gen/file? %)
    #(map :content %)    #(every? not-empty %)
    #(map :content %)    #(every? string? %)
    #(map :class-name %) #(every? not-empty %)
    #(map :class-name %) #(every? string? %)
    #(map :lang %)       #(some #{:clj} %)
    #(map :lang %)       #(some #{:rb} %)
    #(map :lang %)       #(some #{:sc} %))))

(scenario
 ;; Producing Java interfaces for Clojure interop
 (given (produce-java-support-files sample-src
                                    (jall-parse-tree sample-src)
                                    (u/file-langs
                                     (produce-ajvm-files sample-src
                                                         (jall-parse-tree sample-src))))
   (expect
    identity coll?
    identity (fn [coll]
               (every? #(= com.semperos.jall.generate.File (type %)) coll)))))

(scenario
 ;; Transforming the original Java source
 (given (reproduce-java-file sample-src
                             (jall-parse-tree sample-src)
                             (u/file-langs
                              (produce-ajvm-files sample-src
                                                  (jall-parse-tree sample-src))))
   (expect
    identity com.semperos.jall.generate.File
    :lang :java
    :class-name not-empty
    :content not-empty)))

(scenario
 (given (compile-file sample-src)
   (expect
    count 3
    first coll?
    second coll?
    last com.semperos.jall.generate.File
    first (produce-java-support-files sample-src
                                      (jall-parse-tree sample-src)
                                      (u/file-langs (jall-parse-tree sample-src)))
    ;; force evaluation
    #(first (second %)) (first (produce-ajvm-files sample-src
                                                   (jall-parse-tree sample-src))))))