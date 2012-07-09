;; After things get parsed and generated, someone has to be responsible
;; for writing that mess to the file system.
(ns jall.io
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [fs.core :as fs]))

(defn src-lang-folder
  "Mapping of lang to it's sub-folder under `src`, default to 'java' if nothing matches."
  [lang]
  (let [legend {:clj "clojure"
                :rb "ruby"
                :sc "scala"}
        entry (get legend lang)]
    (if (nil? entry)
      "java"
      entry)))

(defn lang-extension
  "Given a language, return the necessary extension. Defaults to '.java'"
  [lang]
  (let [legend {:clj ".clj"
                :rb ".rb"
                :sc ".scala"}
        entry (get legend lang)]
    (if (nil? entry)
      ".java"
      entry)))

(defn file-name
  "Given a `File` record, derive the correct name of the file plus its extension (path not included)"
  [file-record]
  (let [ext (lang-extension (:lang file-record))
        file (second (re-find #"\.([^\.]+)$" (:class-name file-record)))]
    (str file ext)))

(defn necessary-src-dirs
  "Calculate necessary directories based on info in `File` record"
  [file-record]
  (let [lang-folder (src-lang-folder (:lang file-record))
        class-name (:class-name file-record)
        dirs-for-package (butlast (string/split class-name #"\."))
        support-file? (:support-file? file-record)]
    (if support-file?
      (concat ["target" "src" "support" lang-folder] dirs-for-package)
      (concat ["target" "src" "main" lang-folder] dirs-for-package))))

(defn prepare-file
  [project-root file-record]
  (let [dir-path (string/join "/" (concat [project-root] (necessary-src-dirs file-record)))]
    (fs/mkdirs dir-path)
    dir-path))

(defn write-file
  [dir-path file-record]
  (let [file (file-name file-record)
        full-path (string/join "/" [dir-path file])]
    (with-open [w (io/writer full-path)]
      (.write w (:content file-record)))))

(defn prepare-and-write-file
  "Prepare directory structure and write final file based on `File` record"
  [project-root file-record]
  (let [dir-path (prepare-file project-root file-record)]
    (write-file dir-path file-record)))
