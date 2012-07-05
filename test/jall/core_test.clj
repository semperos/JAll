(ns jall.core-test
  (:use clojure.test
        jall.core)
  (:require [net.cgrand.parsley :as p]
            [net.cgrand.parsley.views :as v]))

(def fexpr (p/parser {:main :expr*
                      :space :ws?
                      :root-tag :root
                      :make-node v/fnode
                      :make-leaf v/fleaf}
                :ws #"\s+"
                :expr- #{:vector :list :map :set :symbol}
                :symbol #"[a-zA-Z-]+"
                :vector ["[" :expr* "]"]
                :list ["(" :expr* ")"]
                :map ["{" :expr* "}"]
                :set ["#{" :expr* "}"]))

(def input "(hello #{world kitty})")
(def ftree (fexpr input))

(deftest parsley-views
  (are [v r] (= (v ftree) r)
    v/length (count input)
    v/text input
    (v/view (constantly 0) (fn [_ xs] (reduce + 1 xs))) 8))