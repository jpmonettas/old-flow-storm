(ns flow-storm.all-test
  (:require [clojure.pprint :as pp]
            [flow-storm.api :as fsa]
            [clojure.test :refer [deftest is testing]]
            [flow-storm.tracer :as t]
            [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;
;; Some utilities ;;
;;;;;;;;;;;;;;;;;;;;

(defn clean-clj-fn-print [fp]
  (if (str/starts-with? fp "#object")
    (str/replace fp #"\s.*[a-zA-Z0-9\"\.\$]" "")
    fp))

(defn without-form-flow-id [e]
  (update e 1 dissoc :form-flow-id))

;;;;;;;;;;;
;; Tests ;;
;;;;;;;;;;;

(fsa/trace
 (defn foo [a b]
   (+ a b (or 2 1))))

(fsa/trace
 (defn bar []
   (let [a 10]
     (->> (range (foo a a))
          (map inc)
          (filter odd?)
          (reduce +)))))


(deftest basic-tracing-test
  (let [sent-events (atom [])

        ;; NOTE: we are leaving the :form-flow-id outh since it is random and we can't control
        ;; rand-int in macroexpansions with with-redefs, so we just check that there is a value there
        expected-traces [[:flow-storm/init-trace {:flow-id 1, :form-id 1267089144, :form "(defn bar [] (let [a 10] (->> (range (foo a a)) (map inc) (filter odd?) (reduce +))))", :args-vec "[]", :fn-name "bar"}]
                         [:flow-storm/add-bind-trace {:flow-id 1, :form-id 1267089144, :coor [3], :symbol "a", :value "10"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3 2 4 1], :result #?(:cljs "#object[cljs$core$_PLUS_]" :clj "#object[clojure.core$_PLUS_]")}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3 2 3 1], :result #?(:cljs "#object[cljs$core$odd_QMARK_]" :clj "#object[clojure.core$odd_QMARK_]")}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3 2 2 1], :result #?(:cljs "#object[cljs$core$inc]" :clj "#object[clojure.core$inc]")}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3 2 1 1 1], :result "10"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3 2 1 1 2], :result "10"}]
                         [:flow-storm/init-trace {:flow-id 1, :form-id -828991137, :form-flow-id 27030, :form "(defn foo [a b] (+ a b (or 2 1)))", :args-vec "[10 10]", :fn-name "foo"}]
                         [:flow-storm/add-bind-trace {:flow-id 1, :form-id -828991137, :form-flow-id 27030, :coor nil, :symbol "a", :value "10"}]
                         [:flow-storm/add-bind-trace {:flow-id 1, :form-id -828991137, :form-flow-id 27030, :coor nil, :symbol "b", :value "10"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id -828991137, :form-flow-id 27030, :coor [3 1], :result "10"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id -828991137, :form-flow-id 27030, :coor [3 2], :result "10"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id -828991137, :form-flow-id 27030, :coor [3 3], :result "2"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id -828991137, :form-flow-id 27030, :coor [3], :result "22"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id -828991137, :form-flow-id 27030, :coor [], :result "22", :outer-form? true}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3 2 1 1], :result "22"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3 2 1], :result "(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21)"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3 2 2], :result "(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22)"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3 2 3], :result "(1 3 5 7 9 11 13 15 17 19 21)"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3 2], :result "121"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3], :result "121"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [], :result "121", :outer-form? true}]]]
    (with-redefs [t/ws-send (fn [event] (swap! sent-events conj event))
                  rand-int (constantly 1)]
      (bar)
      (doseq [[et se] (map vector expected-traces @sent-events)]
        (is (-> se second :form-flow-id)
            "Send event doesn't contain a :form-flow-lid")

        (is (= (without-form-flow-id et)
               (cond-> se
                 true without-form-flow-id
                 (contains? (second se) :result) (update-in [1 :result] clean-clj-fn-print)))
            "A generated trace doesn't match with the expected trace")))))

(defn bad-fn []
  #?(:clj (throw (Exception. "error msg"))
     :cljs (throw (js/Error. "error msg"))))

(fsa/trace
 (defn err-foo []
   (->> (range 10)
        (map (fn [i]
               (if (= i 2)
                 (bad-fn)
                 i)))
        doall)))

(deftest exception-tracing-test
  (let [sent-events (atom [])
        obj-result? (fn [m]
                      (and (contains? m :result)
                           (str/starts-with? (:result m) "#object")))
        error-msg #?(:clj "java.lang.Exception: error msg" :cljs "Error: error msg")

        ;; NOTE: we are leaving the :form-flow-id outh since it is random and we can't control
        ;; rand-int in macroexpansions with with-redefs, so we just check that there is a value there
        expected-traces [[:flow-storm/init-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :form "(defn err-foo [] (->> (range 10) (map (fn [i] (if (= i 2) (bad-fn) i))) doall))", :args-vec "[]", :fn-name "err-foo"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1], :result "[stripped-object]"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 1], :result "(0 1 2 3 4 5 6 7 8 9)"}]
                         [:flow-storm/add-bind-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1], :symbol "i", :value "0"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1 2 1 1], :result "0"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1 2 1], :result "false"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1 2 3], :result "0"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1 2], :result "0"}]
                         [:flow-storm/add-bind-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1], :symbol "i", :value "1"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1 2 1 1], :result "1"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1 2 1], :result "false"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1 2 3], :result "1"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1 2], :result "1"}]
                         [:flow-storm/add-bind-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1], :symbol "i", :value "2"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1 2 1 1], :result "2"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1 2 1], :result "true"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1 2 2], :err #:error{:message error-msg}}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1 2], :err #:error{:message error-msg}}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2], :err #:error{:message error-msg}}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3], :err #:error{:message error-msg}}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [], :err #:error{:message error-msg}, :outer-form? true}]]]
    (with-redefs [t/ws-send (fn [event] (swap! sent-events conj event))
                  rand-int (constantly 1)]
      (let [msg (try
                  (err-foo)
                  #?(:clj (catch Exception e (.getMessage e))
                     :cljs (catch :default e (.-message e))))])

      (doseq [[et se] (map vector expected-traces @sent-events)]

        (is (= (without-form-flow-id et)
               (cond-> se
                 true without-form-flow-id
                 (obj-result? (second se)) (assoc-in [1 :result] "[stripped-object]")))
            "A generated trace doesn't match with the expected trace")))))

#trace
(defn zbar [c]
  (+ 5 c))

#ztrace
(defn zfoo [a b]
  (+ a b (zbar a)))

(deftest zero-flow-test
  (let [sent-events (atom [])]
    (with-redefs [t/ws-send (fn [event] (swap! sent-events conj event))]
      (zfoo 42 42)
      
      (is (-> @sent-events first second :fixed-flow-id-starter?)
          ":fixed-flow-id-starter? should be true on the first :flow-storm/init-trace ")
      
      (doseq [[_ {:keys [flow-id]}] @sent-events]
        (is (zero? flow-id) "A zero traced flow-id is not zero.")))))
