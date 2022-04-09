(ns leiningen.new.org.sandbox.scalamixed
  (:require
    [leiningen.new.templates :refer [renderer project-name name-to-path
        multi-segment sanitize-ns ->files *dir*]]
      [leiningen.core.main :as main]
      [clojure.java.shell :as shell]
      [clojure.string :as str]
      [clojure.java.io :as io]
      [clojure.data.json]
      [clojure.walk]
      )
  (:import 
    [java.util.jar JarEntry JarFile]
    [java.lang Thread]
    )
  )

(defn deserialize-str
  [data-str fmt]
  (let [blank-cfg (atom {(keyword "fmt") fmt}) yaml (org.yaml.snakeyaml.Yaml.)
    toml (com.moandjiezana.toml.Toml.)]
    (cond
      (or (= "yaml" fmt) (= "json" fmt)) (let [
          yamlmap (clojure.walk/keywordize-keys (.load yaml data-str))]
        (merge @blank-cfg yamlmap))
      (= "toml" fmt) (let [tomlmap (clojure.walk/keywordize-keys (.toMap (.read toml data-str)))]
        (merge @blank-cfg tomlmap))
      ;(= "json" fmt) (let [jsonmap (clojure.data.json/read-str data-str :key-fn clojure.core/keyword)]
      ; (merge @blank-cfg jsonmap)
      ; )
      )
    )
  )

(defn deserialize-file
  [data-file fmt skel-dir]
  (let [data-strm (atom nil)]
    (try
      (reset! data-strm (io/input-stream (format 
        "resources/leiningen/new/%s/%s" skel-dir data-file)))
    (catch java.io.IOException exc
      (print (format "(exc: %s) Bad file path: resources/leiningen/new/%s/%s\n\n"
        (.getMessage exc) skel-dir data-file))
      (flush)
      (reset! data-strm (io/input-stream (io/resource (format 
        "leiningen/new/%s/%s" skel-dir data-file))))
      ))
    (deserialize-str (String. (.readAllBytes @data-strm)) fmt)
    )
  )

(defn org.sandbox.scalamixed
  "A Leiningen template for mixed Scala project (choices skeleton: skeleton_scala).
  
usage: [LEIN_JVM_OPTS='-Dskeleton=skeleton_scala'] lein new org.sandbox.scalamixed [groupId/]artifactId
LEIN_JVM_OPTS='-DdataFile=data.yaml -DdataFmt=yaml -DffiLib=jna' lein new org.sandbox.scalamixed org.sandbox/intro_jvm.util"
  [raw-name]
  (let [skel-dir (format "org/sandbox/scalamixed/%s" (System/getProperty "skeleton" "skeleton_scala"))
    start-dir (format "%s/{{name}}" skel-dir) render (renderer start-dir)
    data-file (System/getProperty "dataFile" "data.yaml")
    fmt (System/getProperty "dataFmt" "yaml")
    ;main-ns (multi-segment (sanitize-ns raw-name))
    main-ns (multi-segment (str/replace (sanitize-ns raw-name) #"-" "_"))
    initdata (merge (deserialize-file data-file fmt skel-dir)
      {:raw-name raw-name :name (project-name raw-name)
      :namespace main-ns :nesteddirs (name-to-path main-ns)
      :parent (str/join "." (butlast (str/split (project-name raw-name) #"\.")))
      :project (last (str/split (project-name raw-name) #"\."))
      :date (System/getProperty "date" (.format 
        (java.text.SimpleDateFormat. "yyyy-MM-dd") (java.util.Date.)))})
    data (merge initdata
      (if (System/getProperty "groupId")
        {:groupid (System/getProperty "groupId")} {})
      
      {:parentcap (str/capitalize (get initdata :parent))
      :projectcap (str/capitalize (get initdata :project))
      
      (if (System/getProperty "repoHost")
        {:repohost (System/getProperty "repoHost")} {})
      (if (System/getProperty "repoAcct")
        {:repoacct (System/getProperty "repoAcct")} {})
      (if (System/getProperty "date")
        {:year (first (str/split (System/getProperty "date" 
          (get initdata :date)) #"-"))} {})
      (if (System/getProperty "readmeExt")
        {:readmeext (System/getProperty "readmeExt")} {})
      (if (System/getProperty "license")
        {:license (System/getProperty "license")} {})
      (if (System/getProperty "buildTool")
        {:buildtool (System/getProperty "buildTool")} {})
      (if (System/getProperty "testFrwk")
        {:testfrwk (System/getProperty "testFrwk")} {})
      (if (System/getProperty "ffiLib")
        {:ffilib (System/getProperty "ffiLib")} {})
      (if (System/getProperty "executable") 
        {:executable (System/getProperty "executable")} {})
        
      (if (System/getProperty "author")
        {:author (System/getProperty "author")} {})
      (if (System/getProperty "email") 
        {:email (System/getProperty "email")} {})
      )
    files-skel (if (io/resource (format 
        "resources/leiningen/new/%s/%s" data-file skel-dir))
      (map (fn [p] (str/replace p (format "resources/leiningen/new/%s/" 
        start-dir) "")) (filter (fn [f] (.isFile f)) (file-seq (io/file
        (format "resources/leiningen/new/%s" start-dir)))))
      (let [url (first (filter (fn [f] (re-matches #".*\.jar$" 
          (.getPath f))) (seq (.getURLs (-> (Thread/currentThread) 
          (.getContextClassLoader)))))) jFile (JarFile. (.getPath url))]
        (map (fn [p] (str/replace (.toString p) (format 
          "leiningen/new/%s/" start-dir) "")) (filter (fn [e] (and 
          (re-matches (re-pattern (format "leiningen/new/%s/.*" skel-dir)) 
          (.toString e)) (not (re-matches #".*data\.json" (.toString e)))
          (not (re-matches #".*data\.toml" (.toString e)))
          (not (re-matches #".*data\.yaml" (.toString e)))
          )) (filter (fn [e] (not (.isDirectory e))) 
          (enumeration-seq (.entries jFile)))))
        )
      )
    ]
    
    (main/info "Generating fresh 'lein new' org.sandbox.scalamixed project.")
    (main/info (format "... %d files processing ..." (.size files-skel)))
    (apply (partial ->files data) (map (fn [f] 
        [(str/replace f #"\.mustache$" "") (render f data)]) files-skel))
    
    ;(println (:out (apply shell/sh ["echo" "Post generation message"])))
    (->> (apply shell/sh ["echo" "Post generation message"]) :out println)
    (->> (apply shell/sh (str/split (format 
      "groovy -DtemplateGroupId=%s -DtemplateVersion=%s -DtoDir=%s %s/choices/post_gen_project.groovy" 
      (System/getProperty "templateGroupId" "org.sandbox")
      (System/getProperty "templateVersion" "0.1.0") 
      (System/getProperty "toDir" (or *dir* (get data :name)))
      (or *dir* (get data :name))) #" ")) :out println)
    ))
