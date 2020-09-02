(ns tracex.instrument
  (:require [cider.nrepl.middleware.util.instrument :as cider-instrument]))

;; Borrowed from cider-nrepl
(defn walk-indexed
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
     (f coor result))))

(defn interesting? [form]
  (and (list? form)
       (symbol? (first form))))

(defn trace-form [coor traced-form-id form]
  `(let [r# ~form]
     (tracex.tracer/add-trace ~traced-form-id ~coor (quote ~form) r#)
     r#))

(defmacro trace [form]
  (let [form-hash (hash form)]
   `(do
      (tracex.tracer/init-trace ~form-hash (quote ~form))
      ~(walk-indexed (fn [coor sub-form]
                       (if (interesting? sub-form)
                         (trace-form coor form-hash sub-form)
                         sub-form))
                     form))))

(comment

  (trace (let [a (+ 1 2)
               b (+ a a)]
           (map inc (range 10))))

  )
