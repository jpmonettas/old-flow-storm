(ns flow-storm.playground
  (:require [cljs.main :as cljs-main]
            [flow-storm.api :as fsa]
            [flow-storm.instrument :as fsi]
            [clojure.string :as str]
            [clojure.repl :as repl]
            [flow-storm.instrument :as i]
            [clojure.java.io :as io]
            [flowstorm-tester.main :as tester-main]
            [clojure.tools.reader :as reader])
  (:import [java.io PushbackReader]))

(defn- initial-ctx-clj [flow-id form-ns form env]
  (let [form-id (hash form)]
    {
     :on-expr-exec-fn  'flow-storm.tracer/expr-exec-trace
     :on-bind-fn       'flow-storm.tracer/bound-trace
     :on-fn-call-fn    'flow-storm.tracer/fn-call-trace
     :on-outer-form-init-fn 'flow-storm.tracer/init-trace
     :compiler         :clj
     :flow-id          flow-id
     :form-id          form-id
     :form-ns          form-ns
     :disable          #_#{} #{:expr :binding};; :expr :binding
     }))

;;;;;;;;;;;;;;;;;;;
;; By ns tracing ;;
;;;;;;;;;;;;;;;;;;;

(defn redefine-instrumented-form [inst-form form-ns orig-form {:keys [compiler disable] :as ctx}]
  #_(case (fsi/expanded-form-type inst-form ctx)
    :defn (let [{:keys [var-name fn-arities-bodies]} (fsi/parse-defn-expansion inst-form)

                var-ns (when-let [var-ns (namespace var-symb)] (symbol var-ns))
                var-name (symbol (name var-symb))]

            (binding [*ns* (find-ns var-ns)]
              (intern var-ns var-name (eval fn-body))))

    :defmethod (println "WARNING, no defmethod yet")

    :extent-type (println "WARNING, no extend-type yet")

    :extend-protocol (println "WARNING, no extend-protocol yet")

    (let [form-str (pr-str form)]
      (println (format "WARNING, skipping form instrumentation for %s" (subs form-str 0 (min (count form-str) 20))) ))))

(defn trace-var-clj [var-symb]
  (binding [fsi/*environment* {}]
    (let [form (binding [*ns* (find-ns (symbol (namespace var-symb)))]
                 ;; read-string will read namespaced keywords with current *ns*
                 (read-string {:read-cond :allow} (repl/source-fn var-symb)))]
      (if form
        (let [ctx (initial-ctx-clj 0 (namespace var-symb) form nil)
              inst-form (fsi/instrument-all form ctx)]

          (redefine-instrumented-form inst-form var-symb form ctx))

        (throw (Exception. (str "Couldn't find source for " var-symb)))))))

(defn all-ns-with-prefix [prefix {:keys [excluding]}]
  (->> (all-ns)
       (keep (fn [ns]
               (let [nsname (str (ns-name ns))]
                 (when (and (not (excluding nsname))
                            (str/starts-with? nsname prefix))
                  ns))))))

(defn ns-vars [ns]
  (vals (ns-interns ns)))

(defn trace-all-ns-vars [ns stats* {:keys [skip-vars]}]
  (let [vars (ns-vars ns)]
    (swap! stats* assoc-in [(ns-name ns) :of] (count vars))
    (doseq [v vars]
      (if (skip-vars (str (symbol v)))
        (println "SKIPPING" v)
        (try
          (print "Instrumenting" v)
          (trace-var-clj (symbol v))
          (println " ( OK )")
          (swap! stats* update-in [(ns-name ns) :instrumented] inc)
          (catch Exception e
            (println " ERROR: couldn't instrument this code because " (.getMessage e))
            #_(.printStackTrace e)))))))

(defn trace-all-ns [all-ns opts]
  (let [stats* (atom (zipmap (map ns-name all-ns) (repeat {:instrumented 0})))]
    (doseq [ns all-ns]
      (trace-all-ns-vars ns stats* opts))

    (println "\n\n\nSTATS\n")

    (doseq [[ns {:keys [instrumented of]}] @stats*]
      (println (format "%s %d/%d (%d%%)" ns instrumented of (int (/ (* instrumented 100) of))) ))

    (let [{:keys [instrumented of]} (reduce (fn [r n] (merge-with + r n)) (vals @stats*))]
      (println (format "\n\nInstrumented codebase %d/%d (%d%%)" instrumented of (int (/ (* instrumented 100) of)))))))

;;;;;;;;;;;;;;;;;;;;;
;; By file tracing ;;
;;;;;;;;;;;;;;;;;;;;;

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
      (let [macro-expanded-form (fsi/macroexpand-all form ::original-form)
            kind (fsi/expanded-form-type macro-expanded-form {:compiler :clj})]
        (not (contains? #{:defn :defmethod :extend-type :extend-protocol} kind)))))

(defn trace-form [ns form]
  (let [ctx (initial-ctx-clj 0 (str (ns-name ns)) form nil)
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

(defn trace-file-forms [file]
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
                  (trace-form ns form)
                  (print "I")))
               (catch Exception e
                 (println "Error processing form in " ns-from-decl (.getMessage e)))))
           (println)))))))

(defn trace-files-for-namespaces [prefix]
  (let [ns-set (all-ns-with-prefix prefix {:excluding #{""}})
        files-set (interesting-files-for-namespaces ns-set)]
    (doseq [file files-set]
      (trace-file-forms file))))

(comment

  (fsa/connect)

  (trace-all-ns (all-ns-with-prefix "cljs.main"))
  (trace-all-ns (all-ns-with-prefix "cljs."))


  (do
    (def t (Thread. (fn []
                      (binding [flow-storm.tracer/*init-traced-forms* (atom #{})]
                        (cljs-main/-main ["--compile" "hello-world.core"])))))
    (.start t))

  (.stop t)

  (cljs-main/-main "-t" "nodejs" "/home/jmonetta/tmp/cljstest/foo/script.cljs")
  (i/expanded-form-type '(def a))
  )

;; Run with : clj -X flow-storm.playground/runner
(defn runner [& args]
  (fsa/connect)

  (time
   #_(trace-all-ns (all-ns-with-prefix "cljs." {:excluding #{""}})
                   {:skip-vars #{}})
   (trace-files-for-namespaces "cljs.")
   )

  (time
   (binding [flow-storm.tracer/*init-traced-forms* (atom #{})
             flow-storm.tracer/*print-length* 2
             flow-storm.tracer/*flow-id* 0]
     (cljs-main/-main "-t" "nodejs" "/home/jmonetta/tmp/cljstest/foo/script.cljs")))

  #_(System/exit 0)

  ;; -- No instrumentation --
  ;; "Elapsed time: 14886.711692 msecs" ~ 14 secs

  ;; -- Analyzer only FN instrumentation --
  ;; Instrumented codebase 229/308 (74%) "Elapsed time: 2883.123415 msecs" ~ 3 secs
  ;; "Elapsed time: 326844.446424 msecs" ~ 5 min

  ;; -- Full FN instrumentation --
  ;; Instrumented codebase 764/1241 (61%) "Elapsed time: 9712.255063 msecs" ~ 10 secs
  ;; "Elapsed time: 391126.870646 msecs" ~ 6.5 min 21M traces

  ;; -- Full fn instrumentation after thread --
  ;; "Elapsed time: 252373.632364 msecs" ~ 4.2 min
    )

#_(defn runner [& args]

  (fsa/connect)

  (time
   (trace-files-for-namespaces "flowstorm-tester"))

  (time
   (binding [flow-storm.tracer/*init-traced-forms* (atom #{})
             flow-storm.tracer/*print-length* 10
             flow-storm.tracer/*flow-id* 0]
     (tester-main/-main)))

  #_(System/exit 0)

  )
