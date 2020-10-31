(ns flow-storm.instrument
  "This namespace started as a fork of cider.instrument but
  departed a lot from it to make it work for clojurescript and
  to make it able to trace more stuff.

  Provides utilities to recursively instrument forms for all our traces."

  (:require
   clojure.pprint
   [clojure.walk :as walk]
   [cljs.analyzer :as ana]
   [clojure.string :as str]))


;;;;;;;;;;;;;;;;;;;;
;; Some utilities ;;
;;;;;;;;;;;;;;;;;;;;

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; ATTENTION!!, some nasty hacks  ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Normal `clojure.core/macroexpand-1` works differently when being called from clojure and clojurescript. ;;
;; See: https://github.com/jpmonettas/clojurescript-macro-issue                                            ;;
;; One solution is to use clojure.core/macroexpand-1 when we are in a clojure environment                  ;;
;; and user cljs.analyzer/macroexpand-1 when we are in a clojurescript one.                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This will contain the macroexpansion environment
;; the same you can get with the &env in defmacro
(def ^:dynamic *environment*)

(defn target-from-env
  "Given a env map return :cljs or :clj"
  [env]
  (if (contains? env :js-globals)
    :cljs
    :clj))

(defn normalized-macroexpand-1
  "A version of macroexpand-1 that works for clojure and clojurescript
  given it can tell from *environment* if we are in clojure or clojurescript"
  [form]
  ((case (target-from-env *environment*)
     :cljs (partial ana/macroexpand-1 *environment*)
     :clj  macroexpand) form))

(defn normalized-macroexpand
  "A macroexpand version that uses normalized-macroexpand-1 instead of clojure.core/macroexpand-1"
  [form]
  (let [ex (if (seq? form)
             (normalized-macroexpand-1 form)
             form)]
    (if (identical? ex form)
      form
      (normalized-macroexpand ex))))

(defn merge-meta
  "Non-throwing version of (vary-meta obj merge metamap-1 metamap-2 ...).
  Like `vary-meta`, this only applies to immutable objects. For
  instance, this function does nothing on atoms, because the metadata
  of an `atom` is part of the atom itself and can only be changed
  destructively."
  {:style/indent 1}
  [obj & metamaps]
  (try
    (apply vary-meta obj merge metamaps)
    (catch Exception e obj)))

(defn strip-meta
  "Strip meta from form.
  If keys are provided, strip only those keys."
  ([form] (strip-meta form nil))
  ([form keys]
   (if (and (instance? clojure.lang.IObj form)
            (meta form))
     (if keys
       (with-meta form (apply dissoc (meta form) keys))
       (with-meta form nil))
     form)))

(defn macroexpand-all
  "Like `clojure.walk/macroexpand-all`, but preserves and macroexpands
  metadata. Also store the original form (unexpanded and stripped of
  metadata) in the metadata of the expanded form under original-key."
  [form & [original-key]]
  (let [md (meta form)
        expanded (walk/walk #(macroexpand-all % original-key)
                            identity
                            (if (seq? form)
                              ;; Without this, `macroexpand-all`
                              ;; throws if called on `defrecords`.
                              (try (let [r (normalized-macroexpand form)]
                                     ;; TODO: change here
                                     r)
                                   (catch ClassNotFoundException e form))
                              form))]
    (if md
      ;; Macroexpand the metadata too, because sometimes metadata
      ;; contains, for example, functions. This is the case for
      ;; deftest forms.
      (merge-meta expanded
        (macroexpand-all md)
        (when original-key
          ;; We have to quote this, or it will get evaluated by
          ;; Clojure (even though it's inside meta).
          {original-key (list 'quote (strip-meta form))}))

      expanded)))

;;;;;;;;;;;;;;;;;;;;;
;; Instrumentation ;;
;;;;;;;;;;;;;;;;;;;;;

;;; The following code is responsible for automatic instrumentation.
;;; This involves:
;;;    - knowing what's interesting and what's not,
;;;    - walking though the code,
;;;    - distinguishing function calls from special-forms,
;;;    - distinguishing between the different collections.

;;;; ## Auxiliary defs
(def dont-break-forms
  "Set of special-forms that we don't wrap breakpoints around.
  These are either forms that don't do anything interesting (like
  `quote`) or forms that just can't be wrapped (like `catch` and
  `finally`)."
  ;; `recur` needs to be handled separately.
  '#{quote catch finally})

;;;; ## Instrumentation
;;;
;;; The top-level instrumenting function is `instrument-tagged-code`. See
;;; its doc for more information.
;;;
;;; Each of the other `instrument-*` functions is responsible for
;;; calling subordinates and incrementing the coordinates vector if
;;; necessary.

;;;; ### Instrumenting Special forms
;;;
;;; Here, we implement instrumentation of special-forms on a
;;; case-by-case basis. Unlike function calls, we can't just look at
;;; each argument separately.
(declare instrument)

(defmacro definstrumenter
  "Defines a private function for instrumenting forms.
  This is like `defn-`, except the metadata of the return value is
  merged with that of the first input argument."
  [& args]
  (let [[d name f] (macroexpand `(defn- ~@args))]
    `(def ~name
       (fn [& args#] (merge-meta (apply ~f args#) (meta (first args#)))))))

(definstrumenter instrument-coll
  "Instrument a collection."
  [coll ctx]
  (walk/walk #(instrument %1 ctx) identity coll))

(definstrumenter instrument-case-map
  "Instrument the map that is 5th arg in a `case*`."
  [args ctx]
  (into {} (map (fn [[k [v1 v2]]] [k [v1 (instrument v2 ctx)]])
                args)))

(defn- uninteresting-symb?
  "Return true if it is a uninteresting simbol,
  like core.async, generated symbols, the _ symbol, etc."
  [symb]
  (let [symb-name (name symb)]
   (or (= symb-name "_")

       ;; symbols generated by clojure destructure
       (str/includes? symb-name "__")

       ;; core async generated symbols
       (str/includes? symb-name "state_")
       (str/includes? symb-name "statearr-")
       (str/includes? symb-name "inst_"))))

(defn- bind-tracer
  "Generates a form to trace a symbol value at coor."
  [symb coor {:keys [on-bind-fn form-id form-flow-id] :as ctx}]
  (when-not (uninteresting-symb? symb)
    `(~on-bind-fn
      (quote ~symb)
      ~symb
      ~{:coor coor
        :form-id (:form-id ctx)
        :form-flow-id (:form-flow-id ctx)})))

(defn- args-bind-tracers
  "Generates a collection of forms to trace args-vec symbols at coord."
  [args-vec coor ctx]
  (->> args-vec
       (keep (fn [symb] (bind-tracer symb coor ctx)))))


(definstrumenter instrument-special-form
  "Instrument form representing a macro call or special-form."
  [[name & args :as form] ctx]
  (cons name
        ;; We're dealing with some low level stuff here, and some of
        ;; these internal forms are completely undocumented, so let's
        ;; play it safe and use a `try`.
        (try
          (condp #(%1 %2) name
            '#{if do recur throw finally try monitor-exit monitor-enter} (instrument-coll args ctx)
            '#{new} (cons (first args) (instrument-coll (rest args) ctx))
            '#{quote & var clojure.core/import*} args
            '#{.} (list* (first args)
                         ;; To handle the case when second argument to dot call
                         ;; is a list e.g (. class-name (method-name args*))
                         ;; The values present as args* should be instrumented.
                         (let [s (second args)]
                           (if (coll? s)
                             (->> (instrument-coll (rest s) ctx)
                                  (concat (cons (first s) '())))
                             s))
                         (instrument-coll (rest (rest args)) ctx))
            '#{def} (let [sym (first args)]
                      (list* (merge-meta sym
                                           ;; Instrument the metadata, because
                                           ;; that's where tests are stored.
                                           (instrument (or (meta sym) {}) ctx)
                                           ;; to be used later for meta stripping
                                           {::def-symbol true})
                             (map #(instrument % ctx) (rest args))))
            '#{set!} (list (first args)
                           (instrument (second args) ctx))

            ;; trace lets and loops bindings right side recursively
            '#{loop* let* letfn*} (cons (->> (first args)
                                             (partition 2)
                                             (mapcat (fn [[symb x]]
                                                       (let []
                                                         (if (or (uninteresting-symb? symb)
                                                                 (#{'loop*} name))
                                                           [symb (instrument x ctx)]

                                                           ;; if it is not a loop add more _ bindings
                                                           ;; that just trace the bound values
                                                           ;; like [a (+ 1 2)] will became
                                                           ;; [a (+ 1 2)
                                                           ;;  _ (bound-trace a ...)]
                                                           [symb (instrument x ctx)
                                                            '_ (bind-tracer symb (-> form meta ::coor) ctx)]))))
                                             vec)
                                        (instrument-coll (rest args) ctx))
            '#{reify* deftype*} (map #(if (seq? %)
                                        (let [[a1 a2 & ar] %]
                                          (merge-meta (list* a1 a2 (instrument-coll ar ctx))
                                                        (meta %)))
                                        %)
                                     args)
            ;; `fn*` has several possible syntaxes.
            '#{fn*} (let [[a1 & [a2 & ar :as a1r]] args]
                      ;; For all fn* syntaxes instrument their body recursively and also
                      ;; trace the args vector symbols values with bound-trace
                      (cond

                        (vector? a1)       `(~a1 ~@(args-bind-tracers a1 (-> form meta ::coor) ctx) ~@(instrument-coll a1r ctx))

                        (and (symbol? a1)
                             (vector? a2)) `(~a1 ~a2 ~@(args-bind-tracers a1 (-> form meta ::coor) ctx) ~@(instrument-coll a1r ctx))

                        :else              (map #(if (seq? %)
                                                   (-> `(~(first %)
                                                         ~@(args-bind-tracers (first %) (-> form meta ::coor) ctx)
                                                         ~@(instrument-coll (rest %) ctx))
                                                       (merge-meta (meta %)))
                                                   %)
                                                args)))
            '#{catch} `(~@(take 2 args)
                        ~@(instrument-coll (drop 2 args) ctx))
            ;; Anyone know what a2 and a3 represent? They were always 0 on my tests.
            '#{case*} (let [[a1 a2 a3 a4 a5 & ar] args]
                        `(~a1 ~a2 ~a3 ~(instrument a4 ctx) ~(instrument-case-map a5 ctx) ~@ar)))
          (catch Exception e
            (binding [*print-length* 4
                      *print-level*  2]
              (println "Failed to instrument" name args
                       ", please file a bug report: " e))
            args))))

;;;; ### Instrumenting Functions and Collections
;;;
;;; This part is quite simple, most of the code is devoted to checking
;;; form-types and special cases. The idea here is that we walk
;;; through collections and function arguments looking for interesting
;;; things around which we'll wrap a breakpoint. Interesting things
;;; are most function-forms and vars.
(definstrumenter instrument-function-call
  "Instrument a regular function call sexp.
  This must be a sexp that starts with a symbol which is not a macro
  nor a special form.
  This includes regular function forms, like `(range 10)`, and also
  includes calls to Java methods, like `(System/currentTimeMillis)`."
  [[name & args :as fncall] ctx]
  (cons name (instrument-coll args ctx)))

(defn- instrument-form [form orig coor {:keys [instrument-fn form-flow-id form-id outer-form? compiler flow-id] :as ctx}]
  (let [trace-data (cond-> {:coor coor, :form-id form-id :form-flow-id form-flow-id :flow-id flow-id}
                     outer-form? (assoc :outer-form? outer-form?))
        catch-expr (case compiler
                     :clj `(catch Exception e#
                             (~instrument-fn nil
                              {:message (.toString e#)}
                              ~trace-data
                              (quote ~orig))
                             (throw e#))
                     :cljs `(catch :default e#
                              (~instrument-fn nil
                               {:message (.toString e#)}
                               ~trace-data
                               (quote ~orig))
                              (throw e#)))]
    `(try
       (~instrument-fn ~form nil ~trace-data (quote ~orig))
       ~catch-expr)))

;; (defn- instrument-form [form orig coor {:keys [instrument-fn form-flow-id form-id outer-form?]}]
;;   `(~instrument-fn ~form nil
;;     ~(cond-> {:coor coor, :form-id form-id :form-flow-id form-flow-id}
;;        outer-form? (assoc :outer-form? outer-form?))
;;     (quote ~orig)))
 
(defn- maybe-instrument
  "If the form has been tagged with ::coor on its meta, then instrument it
  with trace-and-return"
  ([form {:keys [form-id form-flow-id] :as ctx}]
   (let [{coor ::coor
          [_ orig] ::original-form} (meta form)]
     (cond
       coor
       (instrument-form form orig coor ctx)

       ;; If the form is a list and has no metadata, maybe it was
       ;; destroyed by a macro. Try guessing the extras by looking at
       ;; the first element. This fixes `->`, for instance.
       (seq? form)
       (let [{coor ::coor
              [_ orig] ::original-form} (meta (first form))
             ;; coor (if (= (last extras) 0)
             ;;          (pop extras)
             ;;          extras)
             ]
         (if coor
           (instrument-form form orig coor ctx)
           form))
       :else form))))

(defn- contains-recur?
  "Return true if form is not a `loop` or a `fn` and a `recur` is found in it."
  [form]
  (cond
    (seq? form) (case (first form)
                  recur true
                  loop* false
                  fn*   false
                  (some contains-recur? (rest form)))
    ;; `case` expands the non-default branches into a map.
    ;; We therefore expect a `recur` to appear in a map only
    ;; as the result of a `case` expansion.
    ;; This depends on Clojure implementation details.
    (map? form) (some contains-recur? (vals form))
    (vector? form) (some contains-recur? (seq form))
    :else false))

(defn- dont-break?
  "Return true if it's NOT ok to wrap form in a breakpoint.
  Expressions we don't want to wrap are those listed in
  `dont-break-forms` and anything containing a `recur`
  form (unless it's inside a `loop`)."
  [[name :as form]]
  (or (dont-break-forms name)
      (contains-recur? form)))

(defn- instrument-function-like-form
  "Instrument form representing a function call or special-form."
  [[name :as form] ctx]
  (if-not (symbol? name)
    ;; If the car is not a symbol, nothing fancy is going on and we
    ;; can instrument everything.
    (maybe-instrument (instrument-coll form ctx) ctx)
    (if (special-symbol? name)
      ;; If special form, thread with care.
      (if (dont-break? form)
        (instrument-special-form form ctx)
        (maybe-instrument (instrument-special-form form ctx) ctx))
      ;; Otherwise, probably just a function. Just leave the
      ;; function name and instrument the args.
      (maybe-instrument (instrument-function-call form ctx) ctx))))

(defn- instrument
  "Walk through form and return it instrumented with traces. "
  [form ctx]
  (condp #(%1 %2) form
    ;; Function call, macro call, or special form.
    seq? (doall (instrument-function-like-form form ctx))
    symbol? (maybe-instrument form ctx)
    ;; Other coll types are safe, so we go inside them and only
    ;; instrument what's interesting.
    ;; Do we also need to check for seq?
    coll? (doall (instrument-coll form ctx))
    ;; Other things are uninteresting, literals or unreadable objects.
    form))

(defn- walk-indexed
  "Walk through form calling (f coor element).
  The value of coor is a vector of indices representing element's
  address in the form. Unlike `clojure.walk/walk`, all metadata of
  objects in the form is preserved."
  ([f form] (walk-indexed [] f form))
  ([coor f form]
   (let [map-inner (fn [forms]
                     (map-indexed #(walk-indexed (conj coor %1) f %2)
                                  forms))
         ;; Clojure uses array-maps up to some map size (8 currently).
         ;; So for small maps we take advantage of that, otherwise fall
         ;; back to the heuristic below.
         ;; Maps are unordered, but we can try to use the keys as order
         ;; hoping they can be compared one by one and that the user
         ;; has specified them in that order. If that fails we don't
         ;; instrument the map. We also don't instrument sets.
         ;; This depends on Clojure implementation details.
         walk-indexed-map (fn [map]
                            (map-indexed (fn [i [k v]]
                                           [(walk-indexed (conj coor (* 2 i)) f k)
                                            (walk-indexed (conj coor (inc (* 2 i))) f v)])
                                         map))
         result (cond
                  (map? form) (if (<= (count form) 8)
                                (into {} (walk-indexed-map form))
                                (try
                                  (into (sorted-map) (walk-indexed-map (into (sorted-map) form)))
                                  (catch Exception e
                                    form)))
                  ;; Order of sets is unpredictable, unfortunately.
                  (set? form)  form
                  ;; Borrowed from clojure.walk/walk
                  (list? form) (apply list (map-inner form))
                  (instance? clojure.lang.IMapEntry form) (vec (map-inner form))
                  (seq? form)  (doall (map-inner form))
                  (coll? form) (into (empty form) (map-inner form))
                  :else form)]
     (f coor (merge-meta result (meta form))))))

(defn tag-form
  [coor form]
  (merge-meta form {::coor coor}))

(defn tag-form-recursively
  "Like `tag-form` but also tag all forms inside the given form."
  [form]
  ;; Don't use `postwalk` because it destroys previous metadata.
  (walk-indexed tag-form form))

(defn print-form
  "Pretty print form.
  If expand? is true, macroexpand the form. If meta? is true, also print
  meta. This function is intended for inspection of instrumented code."
  [form & [expand? meta?]]
  (binding [*print-meta* meta?]
    (let [form (if expand?
                 (macroexpand-all form)
                 form)]
      (clojure.pprint/pprint form)))
  (flush)
  form)

(defn- strip-instrumentation-meta
  "Remove all tags in order to reduce java bytecode size and enjoy cleaner code
  printouts."
  [form]
  (walk-indexed
   (fn [_ f]
     (if (instance? clojure.lang.IObj f)
       (let [keys [::original-form ::coor ::def-symbol]
             f    #_(if (::def-symbol (meta f)) ;; TODO: figure this out
                    (let [br (::breakfunction (meta f))
                          f1 (strip-meta f keys)]
                      (if br
                        (vary-meta f1 assoc :cider/instrumented (name (:name (meta br))))
                        f1))
                    (strip-meta f keys))
             (strip-meta f keys)]
         ;; also strip meta of the meta
         (with-meta f (strip-instrumentation-meta (meta f))))
       f))
   form))

(defn instrument-tagged-code
  [form ctx]

  (-> form
      ;; Expand so we don't have to deal with macros.
      (macroexpand-all ::original-form)
      ;; Go through everything again, and instrument any form with
      ;; debug metadata.
      (instrument ctx)
      (strip-instrumentation-meta)))

(defn fn-def-form?
  "Returns true if the form defines a fn.
  Like (def fname (fn* [] ...))"
  [form]
  (when (and (seq? form)
             (= (count form) 3)
             (= 'def (first form)))
    (let [[_ _ x] form]
      (and (seq? x)
           (= (first x) 'fn*)))))

(defn instrument-outer-forms
  "Add some special instrumentation that is needed only on the outer form. Like
  tracing the form source code, and wrapping *flow-id* dynamic bindings"
  [{:keys [orig-form args-vec fn-name form-id form-flow-id flow-id on-outer-form-fn] :as ctx} forms]
  `(binding [flow-storm.tracer/*flow-id* (or ~flow-id
                                             flow-storm.tracer/*flow-id*
                                             ;; TODO: maybe change this to UUID
                                             (rand-int 10000))]
     (~on-outer-form-fn {:form-id ~form-id
                         :flow-id ~flow-id
                         :form-flow-id ~form-flow-id
                         :args-vec ~args-vec
                         :fn-name ~fn-name}
      (quote ~orig-form))

     ~(instrument-form (conj forms 'do) orig-form [] (assoc ctx :outer-form? true))))

;; TODO: can this be mixed with normal fn* body instrumentation?
(defn instrument-function-bodies [[_ & arities] ctx wrapper]
  `(fn*
    ~@(->> arities
         (map (fn [[args-vec & body]]
                (list args-vec (wrapper (assoc ctx :args-vec args-vec) body)))))))

(defn unwrap-form [inst-form]
  (-> inst-form
      second   ;; discard the try
      second)) ;; discard the instrument-fn (trace-and-return)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For working at the repl ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment

  (do
    (require '[flow-storm.tracer :refer [connect]])
    (connect))

  (trace (defn foo [a b]
     (+ a b 10)))

  (trace (let [a (+ 1 2)
               b (+ a a)]
           (->> (range (foo a b))
                (map inc )
                (filter odd?)
                (reduce +))))

  (trace
   (defn factorial [n]
     (if (zero? n)
       1
       (* n (factorial (dec n))))))


  (macroexpand-1 '(trace (let [a 5]
                           (+ a 2))))
  (clojure.core/binding
      [flow-storm.tracer/*form-id* 1234838379]
    (flow-storm.tracer/init-trace
     flow-storm.tracer/*form-id*
     '(let [a 5] (+ a 2)))
    (flow-storm.tracer/add-trace
     (let
         [(flow-storm.tracer/add-trace a {:coor [1 0]}) 5]
       (flow-storm.tracer/add-trace
        (+ (flow-storm.tracer/add-trace a {:coor [2 1]}) 2)
        {:coor [2]}))
     {:coor []}))

  (clojure.core/binding
 [flow-storm.tracer/*form-id* 1234838379]
 (flow-storm.tracer/init-trace
  flow-storm.tracer/*form-id*
  '(let [a 5] (+ a 2)))
 (flow-storm.tracer/add-trace
  (let*
   [a 5]
   (flow-storm.tracer/add-trace
    (+ (flow-storm.tracer/add-trace a {:coor [2 1]}) 2)
    {:coor [2]}))
  {:coor []}))


  )
