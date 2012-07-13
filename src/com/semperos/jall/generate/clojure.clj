(ns com.semperos.jall.generate.clojure
  (:use [clojure.pprint :only [pprint]])
  (:require [com.semperos.jall.util :as u]
            [clojure.string :as string]))

(def clj-method-prefix "java-")

(defn clj-method-sigs
  "The method signatures in :gen-class"
  [methods]
  (vec ;; because I like everything in vectors
   (for [{:keys [name args return-type]} methods]
     (let [genericless-args (into {} (for [[k v] args]
                                       [k (second (re-find #"([^<]+)<?" v))]))]
       [(symbol (u/translate-method-name :clj name)) (vec (map symbol (vals genericless-args))) (symbol return-type)]))))

(defn clj-gen-class
  "The class name is set explicitly to `full-class-name` plus a language-specific suffix."
  [full-class-name methods]
  (list :gen-class
        :name (symbol (u/translate-class-name :clj full-class-name))
        :constructors {[] []}
        :implements [(symbol (u/translate-interface-name :clj full-class-name))]
        :prefix clj-method-prefix))

(defn clj-ns
  [full-class-name import methods]
  (let [klass (symbol (u/translate-class-name :clj full-class-name))
        import-code (read-string (:body import))]
    (if import-code
      (list 'ns klass
            import-code
            (clj-gen-class full-class-name methods))
      (list 'ns klass
            (clj-gen-class full-class-name methods)))))

(defn clj-helpers
  "A helper is a JAll block of possibly multiple function definitions.

   Since `read-string` will only read in the first sexpr it finds, we wrap the input in an extra set of parentheses. For this reason, we `concat` this input."
  [helper-forms]
  (let [helpers (str "(" (:body helper-forms) ")")]
    (read-string helpers)))

(defn clj-defn
  "Generate a `defn` form, again wrapping the body of code in parens and `concat`ing it to make `read-string` read everything in."
  [{:keys [name args body]}]
  (let [wrapped-body (str "(" body ")")
        body-code (read-string wrapped-body)
        starting-defn (list 'defn
                            (symbol (str clj-method-prefix (u/translate-method-name :clj name)))
                            (vec (map symbol (concat ['this] (keys args)))))]
    (concat starting-defn body-code)))

(defn clj-file
  [full-class-name import helpers methods]
  (let [starting-state [(clj-ns full-class-name import methods)]
        state-with-helpers (reduce (fn [state helper-forms]
                                     (concat state (clj-helpers helper-forms)))
                                   starting-state
                                   helpers)]
    (reduce (fn [state method]
              (concat state [(clj-defn method)])) state-with-helpers methods)))

;; Writing to a file should be separate from this generation process
;; [_ dirs file-name] (re-find #"(.*?)\.([^\.]+)$" full-class-name)
;; dirs (string/split dirs #".")
(defn output-file
  [full-class-name import helpers methods]
  (let [pieces (clj-file full-class-name import helpers methods)]
    (string/join "\n"
                 (for [piece pieces]
                   (str (with-out-str (pprint piece)))))))

