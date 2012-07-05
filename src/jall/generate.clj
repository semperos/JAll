;; Functionality for generating AJVM code
;;
;; Coerce values each place they're needed.
;;
(ns jall.generate
  (:use [clojure.pprint :only [pprint]])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [fs.core :as fs]
            [jall.util :as u])
  (:import [java.util.regex Matcher Pattern]))

(defrecord File [lang class-name content support-file?])

(defn init-file
  "Initialize a new `File` record"
  ([] (init-file nil nil nil nil))
  ([lang] (init-file lang nil nil nil))
  ([lang class-name] (init-file lang class-name nil nil))
  ([lang class-name content] (init-file lang class-name content nil))
  ([lang class-name content support-file?] (File. lang class-name content support-file?)))

(def clj-method-prefix "java-")

(declare translate-method-name)
(defn clj-method-sigs
  "The method signatures in :gen-class"
  [methods]
  (vec ;; because I like everything in vectors
   (for [{:keys [name args return-type]} methods]
     ;; only supporting static methods for now
     (let [genericless-args (into {} (for [[k v] args]
                                       [k (second (re-find #"([^<]+)<?" v))]))]
       [(symbol (translate-method-name :clj name)) (vec (map symbol (vals genericless-args))) (symbol return-type)]))))

(declare translate-class-name)
(declare translate-interface-name)
(defn clj-gen-class
  "The class name is set explicitly to `full-class-name` plus a language-specific suffix."
  [full-class-name methods]
  (list :gen-class
        :name (symbol (translate-class-name :clj full-class-name))
        :constructors {[] []}
        :implements [(symbol (translate-interface-name :clj full-class-name))]
        :prefix clj-method-prefix
        ;; :methods (clj-method-sigs methods)
        ))

(defn clj-ns
  [full-class-name methods]
  (let [klass (symbol full-class-name)]
    (list 'ns klass
          (clj-gen-class klass methods))))

(defn clj-defn
  [{:keys [name args body]}]
  (list 'defn
        (symbol (str clj-method-prefix (translate-method-name :clj name)))
        (vec (map symbol (concat ['this] (keys args))))
        (read-string body)))

(defn clj-file
  [full-class-name methods]
  (let [starting-state [(clj-ns full-class-name methods)]]
    (reduce (fn [state method]
              (conj state (clj-defn method))) starting-state methods)))

;; Writing to a file should be separate from this generation process
;; [_ dirs file-name] (re-find #"(.*?)\.([^\.]+)$" full-class-name)
;; dirs (string/split dirs #".")
(defn output-clj-file
  [full-class-name methods]
  (let [pieces (clj-file full-class-name methods)]
    (string/join "\n"
                 (for [piece pieces]
                   (str (with-out-str (pprint piece)))))))

(defn output-file
  "Intermediate fn for output-ajvm-files for dispatching on value of lang"
  [full-class-name lang methods]
  (let [right-methods (filter (fn [state item] (= (:lang item) lang)) methods)]
    (case lang
      :clj (init-file :clj
                      full-class-name
                      (output-clj-file full-class-name methods)))))

(defn output-ajvm-files
  "Given all the method definitions found in the JAll source document, call the appropriate function for transforming method def's for each language"
  [full-class-name methods]
  (let [all-langs (reduce (fn [state item]
                            (conj state (:lang item)))
                          #{}
                          methods)]
    (for [lang all-langs]
      (output-file full-class-name lang methods))))

;; ### Java Support Interfaces ###

(defn java-package
  [full-class-name]
  (let [package (second (re-find #"(.*?)\.[^\.]+$" full-class-name))]
    (format "package %s;\n" package)))

(defn java-interface-decl
  [full-class-name lang]
  (let [interface-name (last (string/split (translate-interface-name lang full-class-name) #"\."))]
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

(defn output-support-for-clj-file
  [full-class-name methods]
  (let [pieces (flatten (support-file-for-lang full-class-name :clj methods))]
    (string/join "\n" pieces)))

(defn output-java-support-file
  "Intermediate fn for output-java-support-files for dispatching on value of lang"
  [full-class-name lang methods]
  (let [right-methods (filter (fn [state item] (= (:lang item) lang)) methods)]
    (case lang
      :clj (init-file :java
                      (translate-interface-name :clj full-class-name)
                      (output-support-for-clj-file full-class-name methods)
                      true ;; support-interface?
                      ))))

(defn output-java-support-files
  [full-class-name methods]
  (let [all-langs (reduce (fn [state item]
                            (conj state (:lang item)))
                          #{}
                          methods)]
    (for [lang all-langs]
      (output-java-support-file full-class-name lang methods))))

;; ### Java equivalents for generated Clojure classes/functions ###

(defn comment-out-positions
  "Return a seq of pairs, representing the starting and ending index (exclusive) that should be commented out."
  [file-content]
  (let [;; can't seem to get a proper negative lookahead working across lines here
        matches (re-seq #"(?s)!def.*?\}\}" file-content)]
    (for [match matches]
      (let [idx-match (.indexOf file-content match)]
        [idx-match
         ;; Plus 2 to account for the two brackets
         (+ 2 (.indexOf file-content "}}" idx-match))]))))

(defn comment-out-regions
  "Given a seq of pairs with begin/end indices for commenting out in a Java file, return the final, concatenated strings with those regions commented out."
  [file-content positions]
  (:output (reduce (fn [state [start end]]
                     (let [old-output (:output state)
                           old-delta  (:delta state)
                           real-start  (+ old-delta start)
                           real-end    (+ old-delta end)
                           initial-sub (.substring old-output 0 real-start)
                           inner-sub   (.substring old-output real-start real-end)
                           post-sub    (.substring old-output real-end)
                           start-comment "\n/*\n"
                           end-comment "\n*/\n"
                           this-delta (count (str start-comment end-comment))]
                       {:output (str initial-sub start-comment inner-sub end-comment post-sub)
                        :delta (+ old-delta this-delta)}))
                   {:output file-content :delta 0}
                   positions)))

(defn comment-out-method-definitions
  [file-content]
  (let [positions (comment-out-positions file-content)]
    (comment-out-regions file-content positions)))

(defn translate-class-name
  "AJVM classes have a suffix appended to make things easier to recognize in generated Java code."
  [lang full-class-name]
  (case (keyword lang)
    :clj (str full-class-name "Clj")
    :rb  (str full-class-name "Rb")
    :sc  (str full-class-name "Sc")))

(defn translate-interface-name
  "AJVM classes implement an auto-generated Java interface with the same methods to ensure typing."
  [lang full-class-name]
  (let [full-name (translate-class-name lang full-class-name)
        parts (string/split full-name #"\.")
        package-portion (butlast parts)
        class-portion (last parts)
        interface-name (str "I" class-portion)]
    (string/join "." (concat package-portion [interface-name]))))

(defn translate-method-name
  "Given a target language and method name, generate the Java class/method that would result"
  [lang method-name]
  (case (keyword lang)
    :clj (u/dashes-to-camel-case method-name)))

(defn replace-method-calls
  "Find calls like `!clj_my-method-here(foo)` and replace them with the correct Java.

   This should probably be handled using things we actually parse out, so we're not parsing all over the place in different ways."
  [file-content full-class-name]
  (let [method-sigs (re-seq #"(?s)!(clj|rb|sc)_([^\(]+)\(" file-content)]
    (reduce (fn [state [whole lang method-name]]
              (let [to-replace (str (apply str (butlast whole)) "\\(")]
                (.replaceAll state to-replace
                             (str
                              "new "
                              (translate-class-name lang full-class-name)
                              "()."
                              (translate-method-name lang method-name)
                              "("))))
            file-content
            method-sigs)))

(defn add-ajvm-imports
  "Given the full class name of the current file and all AJVM languages used, add the necessary import statements to the final Java file"
  [content full-class-name langs]
  (let [imports (reduce (fn [state item]
                          (conj state (str "import " (translate-class-name item full-class-name) ";")))
                        []
                        langs)
        imports-str (string/join "\n" imports)]
    (.replaceFirst content "(?s)\n\\s*import" (str "\n\n" imports-str "\n\nimport"))))

(defn output-java-file
  [full-class-name langs jall-source-file]
  (let [content (slurp jall-source-file)]
    (init-file :java
               full-class-name
               (-> content
                   (comment-out-method-definitions)
                   (replace-method-calls full-class-name)
                   (add-ajvm-imports full-class-name langs)))))