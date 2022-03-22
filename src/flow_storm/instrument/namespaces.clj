(ns flow-storm.instrument.namespaces
  (:require [flow-storm.instrument.forms :as inst-forms]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.tools.reader :as reader]
            [flow-storm.utils :as utils])
  (:import [java.io PushbackReader]))

(defn all-ns-with-prefixes [prefixes {:keys [excluding]}]
  (->> (all-ns)
       (keep (fn [ns]
               (let [nsname (str (ns-name ns))]
                 (when (and (not (excluding nsname))
                            (some (fn [prefix]
                                    (str/starts-with? nsname prefix))
                                  prefixes))
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
      (let [macro-expanded-form (try
                                  (inst-forms/macroexpand-all macroexpand-1 form ::original-form)
                                  (catch Exception e
                                    (let [e-msg (.getMessage e)
                                          ex-type (cond
                                                    ;; core.cljs has a macro called resolve, and also some local fns
                                                    ;; shadowing resolve with a local fn
                                                    ;; When applying clojure.walk/macroexpand-all or our inst-forms/macroexpand-all it doesn't work
                                                    ;; Hard to fix, and there shouldn't be a lot of cases like that
                                                    (and (= (ns-name ns) 'cljs.core)
                                                         (str/includes? e-msg "macroexpanding resolve"))
                                                    :known-error

                                                    :else
                                                    :unknown-error)]
                                      (throw (ex-info "Error macroexpanding form" {:type ex-type})))))
            kind (inst-forms/expanded-form-type macro-expanded-form {:compiler :clj})]
        (not (contains? #{:defn :defmethod :extend-type :extend-protocol} kind)))))

(defn trace-form [ns form config]
  (let [ctx (inst-forms/build-form-instrumentation-ctx config (str (ns-name ns)) form nil)
        inst-form (try
                    (-> form
                        (inst-forms/instrument-all ctx)
                        (inst-forms/maybe-unwrap-outer-form-instrumentation ctx))
                    (catch Exception e
                      (throw (ex-info "Error instrumenting form" {:type :unknown-error}))))]

    (try
      (eval inst-form)
      (catch Exception e
        (let [e-msg (.getMessage e)
              ex-type (cond

                        ;; known issue, using recur inside fn* (without loop*)
                        (str/includes? e-msg "recur")
                        :known-error

                        :else
                        :unknown-error)]
          (throw (ex-info "Error evaluating form" {:type ex-type})))))))

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
  #_(print "About to instrument file " (.getFile file))
  (let [[_ ns-from-decl] (read-file-ns-decl file)]
    (if-not ns-from-decl

      (println (format "Warning, skipping %s since it doesn't contain a (ns ) decl. We don't support (in-ns ...) yet." (.getFile file)))

      ;; this is IMPORTANT, once we have `ns-from-decl` all the instrumentation work
      ;; should be done as if we where in `ns-from-decl`
      (binding [*ns* (find-ns ns-from-decl)
                inst-forms/*environment* {}]
        (when-not (= ns-from-decl 'clojure.core) ;; we don't want to instrument clojure core since it brings too much noise
         (let [ns (find-ns ns-from-decl)
               file-forms (read-string {:read-cond :allow}
                                       (format "[%s]" (slurp file)))]
           (println (format "Instrumenting namespace: %s Forms (%d) (%s)" ns-from-decl (count file-forms) (.getFile file)))

           (doseq [form file-forms]
             (try

               (if (uninteresting-form? ns form)
                 (print ".")

                 (do
                   (trace-form ns form config)
                   (print "I")))

               (catch clojure.lang.ExceptionInfo ei
                 (let [ex-type (:type (ex-data ei))]
                   ;; Enable for debugging unknown errors
                   #_(when (= ex-type :unknown-error)
                       (println (ex-message ei))
                       (System/exit 1))
                   (case ex-type
                       :known-error   (print (utils/colored-string "X" :yellow))
                       :unknown-error (print (utils/colored-string "X" :red)))))))
           (println)))))))

(defn trace-files-for-namespaces [prefixes config]
  (let [ns-set (all-ns-with-prefixes prefixes {:excluding #{""}})
        files-set (interesting-files-for-namespaces ns-set)]
    (doseq [file files-set]
      (trace-file-forms file config))))
