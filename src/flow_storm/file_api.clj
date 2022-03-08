(ns flow-storm.file-api
  (:require [cljs.main :as cljs-main]
            [flow-storm.api :as fsa]
            [flow-storm.tracer :as fst]
            [flow-storm.instrument :as fsi]
            [clojure.string :as str]
            [clojure.repl :as repl]
            [flow-storm.instrument :as i]
            [clojure.java.io :as io]
            [clojure.tools.reader :as reader])
  (:import [java.io PushbackReader]))

(defn all-ns-with-prefix [prefix {:keys [excluding]}]
  (->> (all-ns)
       (keep (fn [ns]
               (let [nsname (str (ns-name ns))]
                 (when (and (not (excluding nsname))
                            (str/starts-with? nsname prefix))
                  ns))))))

(defn ns-vars [ns]
  (vals (ns-interns ns)))

(defn interesting-files-for-namespaces
  "Try to find as much project files as possible for namespaces that start with `prefix`."
  [ns-set]
  (reduce (fn [r ns]
            (let [ns-vars (vals (ns-interns ns))]
              (into r (keep (fn [v]
                              (when-let [file (:file (meta v))]
                                (io/resource file)))
                            ns-vars))))
          #{}
          ns-set))

(defn uninteresting-form? [ns form]
  (or (nil? form)
      (when (and (seq? form)
                 (symbol? (first form)))
        (contains? '#{"ns" "defrecord" "defmulti" "deftype"
                      "defprotocol" "defmacro" "comment" "import-macros"}
                   (name (first form))))
      (let [macro-expanded-form (fsi/macroexpand-all macroexpand-1 form ::original-form)
            kind (fsi/expanded-form-type macro-expanded-form {:compiler :clj})]
        (not (contains? #{:defn :defmethod :extend-type :extend-protocol} kind)))))

(defn trace-form [ns form config]
  (let [ctx (fsa/build-form-instrumentation-ctx config (str (ns-name ns)) form nil)
        inst-form (try
                    (-> form
                        (fsi/instrument-all ctx)
                        (fsi/maybe-unwrap-outer-form-instrumentation ctx))
                    (catch Exception e
                      (prn "Error instrumenting form " form)
                      (.printStackTrace e)
                      (System/exit 1)))]

    (try
      (eval inst-form)
      (catch Exception e
        (println "Error evaluating form")
        #_(prn "Error evaluating form" inst-form)
        #_(prn "NS" (ns-name ns) "original form " form)
        #_(.printStackTrace e)
        #_(System/exit 1)))))

(defn read-file-ns-decl
  "Attempts to read a (ns ...) declaration from file, and returns the
  unevaluated form. Returns nil if ns declaration cannot be found.
  read-opts is passed through to tools.reader/read."
  [file]
  (let [ns-decl? (fn [form] (and (list? form) (= 'ns (first form))))]
    (with-open [rdr (PushbackReader. (io/reader file))]
     (let [opts {:read-cond :allow
                 :features #{:clj}
                 :eof ::eof}]
       (loop []
         (let [form (reader/read opts rdr)]
           (cond
             (ns-decl? form) form
             (= ::eof form) nil
             :else (recur))))))))

(defn trace-file-forms [file config]
  (print "About to instrument file " (.getFile file))
  (let [[_ ns-from-decl] (read-file-ns-decl file)]
    (println " NS " ns-from-decl)
    (if-not ns-from-decl

      (println (format "Warning, skipping %s since it doesn't contain a (ns ) decl. We don't support (in-ns ...) yet." (.getFile file)))

      ;; this is IMPORTANT, once we have `ns-from-decl` all the instrumentation work
      ;; should be done as if we where in `ns-from-decl`
      (binding [*ns* (find-ns ns-from-decl)
                fsi/*environment* {}]
        (when-not (= ns-from-decl 'clojure.core) ;; we don't want to instrument clojure core since it brings too much noise
         (let [ns (find-ns ns-from-decl)
               file-forms (read-string {:read-cond :allow}
                                       (format "[%s]" (slurp file)))]
           (println (format "NS for file %s File contains %d forms" ns-from-decl (count file-forms)))

           (doseq [form file-forms]
             (try
               (if (uninteresting-form? ns form)
                (print ".")

                (do
                  (trace-form ns form config)
                  (print "I")))
               (catch Exception e
                 (println "Error processing form in " ns-from-decl (.getMessage e)))))
           (println)))))))

(defn trace-files-for-namespaces [prefix config]
  (let [ns-set (all-ns-with-prefix prefix {:excluding #{""}})
        files-set (interesting-files-for-namespaces ns-set)]
    (doseq [file files-set]
      (trace-file-forms file config))))


;; Run with : clj -X flow-storm.file-api/cljs-test
(defn cljs-test [& args]
  (fsa/connect)

  (time
   (trace-files-for-namespaces "cljs." {:disable #{:expr :binding}})
   )

  (time
   (fsa/run-with-execution-ctx
    {:flow-id 0
     :print-length 2
     :print-level 1}
    (cljs-main/-main "-t" "nodejs" "/home/jmonetta/tmp/cljstest/foo/script.cljs")))

  #_(System/exit 0)
    )
