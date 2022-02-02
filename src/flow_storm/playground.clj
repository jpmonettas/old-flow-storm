(ns flow-storm.playground
  (:require [cljs.main :as cljs-main]
            [flow-storm.api :as fsa]
            [flow-storm.instrument :as fsi]
            [clojure.string :as str]
            [clojure.repl :as repl]))

(defn- initial-ctx-clj [form]
  (let [form-id (hash form)]
    {:on-expr-exec-fn    'flow-storm.tracer/expr-exec-trace
     :on-bind-fn       'flow-storm.tracer/bound-trace
     :on-fn-call-fn    'flow-storm.tracer/fn-call-trace
     :on-outer-form-fn 'flow-storm.tracer/init-trace
     :compiler :clj
     :form-id          form-id}))

(defn redefine-vars-clj [inst-form var-symb orig-form {:keys [compiler] :as ctx}]
  (let [outer-form (fsi/unwrap-instrumentation inst-form)]
    (if (= (fsi/outer-form-type outer-form ctx) :defn)

      (let [{:keys [fn-body fn-name]} (fsi/parse-defn-expansion outer-form)
            fn-body (fsi/instrument-function-bodies
                     fn-body
                     (assoc ctx
                            :orig-form orig-form
                            :fn-name (name fn-name))
                     fsi/instrument-outer-forms)
            var-ns (when-let [var-ns (namespace var-symb)] (symbol var-ns))
            var-name (symbol (name var-symb))]

        (binding [*ns* (find-ns var-ns)]
          (intern var-ns var-name (eval fn-body))))

      (let [ofs (pr-str outer-form) ]
        (throw (Exception. (str "flow-storm only support fn tracing now. " (subs ofs 0 (min (count ofs) 20)))))))))

(defn trace-var-clj [var-symb]
  (binding [fsi/*environment* {}]
    (let [form (read-string {:read-cond :allow} (repl/source-fn var-symb))]
      (if form
        (let [ctx (initial-ctx-clj form)
              inst-form (fsi/instrument-all form ctx)]

          (redefine-vars-clj inst-form var-symb form ctx))

        (throw (Exception. (str "Couldn't find source for " var-symb)))))))

(defn all-ns-with-prefix [prefix]
  (->> (all-ns)
       (keep (fn [ns]
               (when (str/starts-with? (str (ns-name ns)) prefix)
                 ns)))))

(defn ns-vars [ns]
  (vals (ns-interns ns)))

(defn trace-all-ns-vars [ns stats*]
  (let [vars (ns-vars ns)]
    (swap! stats* assoc-in [(ns-name ns) :of] (count vars))
    (doseq [v vars]
      (try
        (print "Instrumenting" (symbol v))
        (trace-var-clj (symbol v))
        (println " ( OK )")
        (swap! stats* update-in [(ns-name ns) :instrumented] inc)
        (catch Exception e
          (println " ERROR: couldn't instrument this code because " (.getMessage e)))))))

(defn trace-all-ns [all-ns]
  (let [stats* (atom (zipmap (map ns-name all-ns) (repeat {:instrumented 0})))]
    (doseq [ns all-ns]
      (trace-all-ns-vars ns stats*))

    (println "\n\n\nSTATS\n")

    (doseq [[ns {:keys [instrumented of]}] @stats*]
      (println (format "%s %d/%d (%d%%)" ns instrumented of (int (/ (* instrumented 100) of))) ))

    (let [{:keys [instrumented of]} (reduce (fn [r n] (merge-with + r n)) (vals @stats*))]
      (println (format "\n\nInstrumented codebase %d/%d (%d%%)" instrumented of (int (/ (* instrumented 100) of)))))))

(comment

  (fsa/connect)


  (-> (all-ns-with-prefix "cljs.main")
      first
      trace-all-ns-vars)

  (trace-all-ns (all-ns-with-prefix "cljs."))

  (cljs-main/-main ["--compile" "hello-world.core"])





  )
