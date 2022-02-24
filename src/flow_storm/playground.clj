(ns flow-storm.playground
  (:require [cljs.main :as cljs-main]
            [flow-storm.api :as fsa]
            [flow-storm.instrument :as fsi]
            [clojure.string :as str]
            [clojure.repl :as repl]))

(defn- initial-ctx-clj [form]
  (let [form-id (hash form)]
    {
     :on-expr-exec-fn  'flow-storm.tracer/expr-exec-trace
     :on-bind-fn       'flow-storm.tracer/bound-trace
     :on-fn-call-fn    'flow-storm.tracer/fn-call-trace
     :on-outer-form-fn 'flow-storm.tracer/init-trace
     :compiler         :clj
     :disable          #{:expr :binding} ;; :expr :binding
     :form-id          form-id}))

(defn redefine-vars-clj [inst-form var-symb orig-form {:keys [compiler disable] :as ctx}]
  (let [outer-form (if (disable :expr) inst-form (fsi/unwrap-instrumentation inst-form))]
    (if (= (fsi/outer-form-type outer-form ctx) :defn)

      (let [{:keys [fn-body fn-name]} (fsi/parse-defn-expansion outer-form)
            fn-body (fsi/instrument-function-bodies
                     fn-body
                     (assoc ctx
                            :form-ns (namespace var-symb)
                            :orig-form orig-form
                            :fn-name (str fn-name))
                     fsi/instrument-outer-forms)
            var-ns (when-let [var-ns (namespace var-symb)] (symbol var-ns))
            var-name (symbol (name var-symb))]

        (binding [*ns* (find-ns var-ns)]
          (intern var-ns var-name (eval fn-body))))

      (let [ofs (pr-str outer-form) ]
        (throw (Exception. (str "flow-storm only support fn tracing now. " (subs ofs 0 (min (count ofs) 20)))))))))

(defn trace-var-clj [var-symb]
  (binding [fsi/*environment* {}]
    (let [form (binding [*ns* (find-ns (symbol (namespace var-symb)))]
                 ;; read-string will read namespaced keywords with current *ns*
                 (read-string {:read-cond :allow} (repl/source-fn var-symb)))]
      (if form
        (let [ctx (initial-ctx-clj form)
              inst-form (fsi/instrument-all form ctx)]

          (redefine-vars-clj inst-form var-symb form ctx))

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

  )

;; Run with : clj -X flow-storm.playground/runner
(defn runner [& args]
  (fsa/connect)

  (time
   (trace-all-ns (all-ns-with-prefix "cljs." {:excluding #{""}})
                 {:skip-vars #{}}))

  (time
   (binding [flow-storm.tracer/*init-traced-forms* (atom #{})
             flow-storm.tracer/*print-length* 2
             flow-storm.tracer/*flow-id* 0]
     (cljs-main/-main "-t" "nodejs" "/home/jmonetta/tmp/cljstest/foo/script.cljs")))

  (System/exit 0)

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
