(ns jall.generate.clojure
  (:use [clojure.pprint :only [pprint]])
  (:require [jall.util :as u]
            [clojure.string :as string]))

(def clj-method-prefix "java-")

(defn clj-method-sigs
  "The method signatures in :gen-class"
  [methods]
  (vec ;; because I like everything in vectors
   (for [{:keys [name args return-type]} methods]
     ;; only supporting static methods for now
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
        :prefix clj-method-prefix
        ;; :methods (clj-method-sigs methods)
        ))

(defn clj-ns
  [full-class-name import methods]
  (let [klass (symbol full-class-name)
        import-code (read-string (:body import))]
    (if import-code
      (list 'ns klass
            import-code
            (clj-gen-class klass methods))
      (list 'ns klass
            (clj-gen-class klass methods)))))

(defn clj-defn
  [{:keys [name args body]}]
  (list 'defn
        (symbol (str clj-method-prefix (u/translate-method-name :clj name)))
        (vec (map symbol (concat ['this] (keys args))))
        (read-string body)))

(defn clj-file
  [full-class-name import methods]
  (let [starting-state [(clj-ns full-class-name import methods)]]
    (reduce (fn [state method]
              (conj state (clj-defn method))) starting-state methods)))

;; Writing to a file should be separate from this generation process
;; [_ dirs file-name] (re-find #"(.*?)\.([^\.]+)$" full-class-name)
;; dirs (string/split dirs #".")
(defn output-file
  [full-class-name import methods]
  (let [pieces (clj-file full-class-name import methods)]
    (string/join "\n"
                 (for [piece pieces]
                   (str (with-out-str (pprint piece)))))))

