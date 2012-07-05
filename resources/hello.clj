(ns
 com.example.Hello
 (:gen-class
  :name
  com.example.Hello
  :constructors
  [[]]
  :prefix
  "java-"
  :methods
  [[method-foo [arg1 arg2] nil]
   [method-bar [persons-name] java.lang.String]]))

(defn
 java-method-foo
 [arg1 arg2]
 (println (str "Wow, this is cool: " arg1 arg2)))

(defn
 java-method-bar
 [persons-name]
 (let [greeting "Hello, "] (str greeting persons-name)))

