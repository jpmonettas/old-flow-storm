(ns flow-storm.debugger.form-pprinter
  (:require [clojure.pprint :as pp]))

(defn- seq-delims [form]
  (let [delims (pr-str (empty form))]
    (if (= (count delims) 2)
      [(str (first delims)) (str (second delims))]
      ["#{" "}"])))

(defn- form-tokens
  ([form] (form-tokens form []))
  ([form curr-coord]
   (cond
     (or (seq? form) (vector? form))
     (let [[db de] (seq-delims form)]
       (-> [[db curr-coord]]
           (into (mapcat (fn [i f] (form-tokens f (conj curr-coord i))) (range) form))
           (into [[de curr-coord]])))

     (map? form)
     (-> [["{" curr-coord]]
         (into (mapcat (fn [i f] (form-tokens f (conj curr-coord i))) (range) (mapcat identity form)))
         (into [["}" curr-coord]]))

     :else
     [[(pr-str form) curr-coord]])))

(defn- consecutive-inv-chars [inv-chars-map idx]
  (loop [i (inc idx)
         inv-chars [(inv-chars-map idx)]]
    (if-let [inv-char (inv-chars-map i)]
      (recur (inc i) (conj inv-chars inv-char))
      inv-chars)))

(defn pprint-tokens [form]
  (let [pprinted-str (with-out-str
                       (binding [pp/*print-pprint-dispatch* pp/code-dispatch]
                         (pp/pprint form)))
        pos->layout-char (->> pprinted-str
                              (keep-indexed (fn [i c] (cond
                                                        (= c \newline) [i :nl]
                                                        (= c \space)   [i :sp]
                                                        :else nil)))
                              (into {}))
        pre-tokens (form-tokens form)]
    (loop [[[tname :as tok] & next-tokens] pre-tokens
           i 0
           final-toks []]
      (if-not tok
        final-toks
        (if (pos->layout-char i)
          (let [consec-inv-chars (consecutive-inv-chars pos->layout-char i)]
            (recur next-tokens
                   (+ i (count tname) (count consec-inv-chars))
                   (-> final-toks
                       (into consec-inv-chars)
                       (into  [tok]))))
          (recur next-tokens (+ i (count tname)) (into final-toks [tok])))))))

(defn- debug-print-tokens [ptokens]
  (doseq [t ptokens]
    (cond
      (= :sp t) (print " ")
      (= :nl t) (println)
      :else     (print (first t))))
  (println))

(comment

  (let [test-form '(defn factorial [n] (if (zero? n) 1 (* n (factorial (dec n)))))]
    (binding [pp/*print-right-margin* 40
              pp/*print-pprint-dispatch* pp/code-dispatch]
      (= (-> test-form
             (pprint-tokens)
             debug-print-tokens
             with-out-str)
         (-> test-form
             pp/pprint
             with-out-str))))
  )
