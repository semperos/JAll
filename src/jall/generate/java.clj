(ns jall.generate.java
  (:require [jall.util :as u]
            [clojure.string :as string]))

(defn comment-out-positions
  "Return a seq of pairs, representing the starting and ending index (exclusive) that should be commented out."
  [file-content]
  (let [;; can't seem to get a proper negative lookahead working across lines here
        import-matches (re-seq #"(?s)!import.*?\}\}" file-content)
        def-matches (re-seq #"(?s)!def.*?\}\}" file-content)
        matches (concat import-matches def-matches)]
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

(defn replace-method-calls
  "Find calls like `!clj_my-method-here(foo)` and replace them with the correct Java.

   TODO: This should probably be handled using things we actually parse out, so we're not parsing all over the place in different ways."
  [file-content full-class-name]
  (let [method-sigs (re-seq #"(?s)!(clj|rb|sc)_([^\(]+)\(" file-content)]
    (reduce (fn [state [whole lang method-name]]
              (let [to-replace (str (apply str (butlast whole)) "\\(")]
                (if (.contains method-name ".")
                  ;; if there's a dot, it means this is a fully-qualified reference to another class
                  (let [other-class (second (re-find #"([^/]+)/" method-name))
                        real-method-name (second (re-find #"/(.*)$" method-name))]
                    (.replaceAll state to-replace
                                 (str
                                  "new "
                                  (u/translate-class-name lang other-class)
                                  "()."
                                  (u/translate-method-name lang real-method-name)
                                  "(")))
                  (.replaceAll state to-replace
                               (str
                                "new "
                                (u/translate-class-name lang full-class-name)
                                "()."
                                (u/translate-method-name lang method-name)
                                "(")))))
            file-content
            method-sigs)))

(defn add-ajvm-imports
  "Given the full class name of the current file and all AJVM languages used, add the necessary import statements to the final Java file"
  [content full-class-name langs]
  (let [imports (reduce (fn [state item]
                          (conj state (str "import " (u/translate-class-name item full-class-name) ";")))
                        []
                        langs)
        imports-str (string/join "\n" imports)]
    (.replaceFirst content "(?s)\n\\s*import" (str "\n\n" imports-str "\n\nimport"))))

