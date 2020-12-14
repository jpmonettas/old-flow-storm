(ns flow-storm.all-test
  (:require [clojure.pprint :as pp]
            [flow-storm.api :as fsa]
            [clojure.test :refer [deftest is testing #?(:cljs async)]]
            [flow-storm.tracer :as t]
            [clojure.string :as str]
            [editscript.core :as edit.core]
            [editscript.edit :as edit.edit]
            #?(:clj [clojure.java.shell :as shell])
            #?(:cljs [cljs.tools.reader :as tools-reader])))

;;;;;;;;;;;;;;;;;;;;
;; Some utilities ;;
;;;;;;;;;;;;;;;;;;;;

(defn clean-clj-fn-print [fp]
  (if (str/starts-with? fp "#object")
    (str/replace fp #"\s.*[a-zA-Z0-9\"\.\$]" "")
    fp))

(defn event-data-without [e k]
  (update e 1 dissoc k))

#?(:clj (def read-str read-string)
   :cljs (def read-str tools-reader/read-string))

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
        expected-traces [[:flow-storm/init-trace {:flow-id 1, :form-id 1267089144, :form "(defn bar [] (let [a 10] (->> (range (foo a a)) (map inc) (filter odd?) (reduce +))))", :args-vec "[]", :fn-name "bar" :timestamp 1}]
                         [:flow-storm/add-bind-trace {:flow-id 1, :form-id 1267089144, :timestamp 1, :coor [3], :symbol "a", :value "10"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3 2 4 1], :result #?(:cljs "#object[cljs$core$_PLUS_]" :clj "#object[clojure.core$_PLUS_]") :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3 2 3 1], :result #?(:cljs "#object[cljs$core$odd_QMARK_]" :clj "#object[clojure.core$odd_QMARK_]") :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3 2 2 1], :result #?(:cljs "#object[cljs$core$inc]" :clj "#object[clojure.core$inc]") :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3 2 1 1 1], :result "10" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3 2 1 1 2], :result "10" :timestamp 1}]
                         [:flow-storm/init-trace {:flow-id 1, :form-id -828991137, :form-flow-id 27030, :form "(defn foo [a b] (+ a b (or 2 1)))", :args-vec "[10 10]", :fn-name "foo" :timestamp 1}]
                         [:flow-storm/add-bind-trace {:flow-id 1, :form-id -828991137, :form-flow-id 27030, :timestamp 1, :coor nil, :symbol "a", :value "10"}]
                         [:flow-storm/add-bind-trace {:flow-id 1, :form-id -828991137, :form-flow-id 27030, :timestamp 1, :coor nil, :symbol "b", :value "10"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id -828991137, :form-flow-id 27030, :coor [3 1], :result "10" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id -828991137, :form-flow-id 27030, :coor [3 2], :result "10" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id -828991137, :form-flow-id 27030, :coor [3 3], :result "2" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id -828991137, :form-flow-id 27030, :coor [3], :result "22" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id -828991137, :form-flow-id 27030, :coor [], :result "22", :outer-form? true :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3 2 1 1], :result "22" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3 2 1], :result "(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21)" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3 2 2], :result "(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22)" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3 2 3], :result "(1 3 5 7 9 11 13 15 17 19 21)" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3 2], :result "121" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3], :result "121" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [], :result "121", :outer-form? true :timestamp 1}]]]

    (with-redefs [t/get-timestamp (constantly 1)
                  t/ws-send (fn [event] (swap! sent-events conj event))
                  rand-int (constantly 1)]
      (bar)
      
      (doseq [[et se] (map vector expected-traces @sent-events)]
        (is (-> se second :form-flow-id)
            "Send event doesn't contain a :form-flow-lid")

        (is (= (event-data-without et :form-flow-id)
               (cond-> se
                 true (event-data-without :form-flow-id)
                 (contains? (second se) :result) (update-in [1 :result] clean-clj-fn-print)))
            "A generated trace doesn't match with the expected trace")))))

(defmulti multi-foo :type)

#trace
(defmethod multi-foo :square [{:keys [n]}]
  (* n n))

#trace
(defmethod multi-foo :twice [{:keys [n]}]
  (* 2 n))

(deftest multimethods-trace-test
  (let [sent-events (atom [])

        ;; NOTE: we are leaving the :form-flow-id outh since it is random and we can't control
        ;; rand-int in macroexpansions with with-redefs, so we just check that there is a value there
        expected-traces [[:flow-storm/init-trace {:flow-id 1, :form-id 1746119536, :form-flow-id 75891, :form "(defmethod multi-foo :square [{:keys [n]}] (* n n))", :args-vec "[{:type :square, :n 5}]", :fn-name "multi-foo" :timestamp 1}]
                         [:flow-storm/add-bind-trace {:flow-id 1, :form-id 1746119536, :form-flow-id 75891, :timestamp 1, :coor nil, :symbol "n", :value "5"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1746119536, :form-flow-id 75891, :coor [4 1], :result "5" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1746119536, :form-flow-id 75891, :coor [4 2], :result "5" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1746119536, :form-flow-id 75891, :coor [4], :result "25" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1746119536, :form-flow-id 75891, :coor [], :result "25", :outer-form? true :timestamp 1}]

                         [:flow-storm/init-trace {:flow-id 1, :form-id 872455023, :form-flow-id 76669, :form "(defmethod multi-foo :twice [{:keys [n]}] (* 2 n))", :args-vec "[{:type :twice, :n 5}]", :fn-name "multi-foo" :timestamp 1}]
                         [:flow-storm/add-bind-trace {:flow-id 1, :form-id 872455023, :form-flow-id 76669, :timestamp 1, :coor nil, :symbol "n", :value "5"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 872455023, :form-flow-id 76669, :coor [4 2], :result "5" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 872455023, :form-flow-id 76669, :coor [4], :result "10" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 872455023, :form-flow-id 76669, :coor [], :result "10", :outer-form? true :timestamp 1}]]]
    (with-redefs [t/get-timestamp (constantly 1)
                  t/ws-send (fn [event] (swap! sent-events conj event))
                  rand-int (constantly 1)]

      (multi-foo {:type :square :n 5})
      (multi-foo {:type :twice :n 5})

      (doseq [[et se] (map vector expected-traces @sent-events)]
        (is (= (event-data-without et :form-flow-id)
               (event-data-without se :form-flow-id))
            "A generated trace doesn't match with the expected trace")))))

#trace
(defn multi-arity-foo
  ([a] (multi-arity-foo a 10))
  ([a b] (+ a b)))

(deftest multi-arity-test
  (let [sent-events (atom [])

        ;; NOTE: we are leaving the :form-flow-id outh since it is random and we can't control
        ;; rand-int in macroexpansions with with-redefs, so we just check that there is a value there
        expected-traces [[:flow-storm/init-trace {:flow-id 1, :form-id 1326303898, :form-flow-id 24689, :form "(defn multi-arity-foo ([a] (multi-arity-foo a 10)) ([a b] (+ a b)))", :args-vec "[5]", :fn-name "multi-arity-foo" :timestamp 1}]
                         [:flow-storm/add-bind-trace {:flow-id 1, :form-id 1326303898, :form-flow-id 24689, :timestamp 1, :coor nil, :symbol "a", :value "5"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1326303898, :form-flow-id 24689, :coor [2 1 1], :result "5" :timestamp 1}]
                         [:flow-storm/init-trace {:flow-id 1, :form-id 1326303898, :form-flow-id 24689, :form "(defn multi-arity-foo ([a] (multi-arity-foo a 10)) ([a b] (+ a b)))", :args-vec "[5 10]", :fn-name "multi-arity-foo" :timestamp 1}]
                         [:flow-storm/add-bind-trace {:flow-id 1, :form-id 1326303898, :form-flow-id 24689, :timestamp 1, :coor nil, :symbol "a", :value "5"}]
                         [:flow-storm/add-bind-trace {:flow-id 1, :form-id 1326303898, :form-flow-id 24689, :timestamp 1, :coor nil, :symbol "b", :value "10"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1326303898, :form-flow-id 24689, :coor [3 1 1], :result "5" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1326303898, :form-flow-id 24689, :coor [3 1 2], :result "10" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1326303898, :form-flow-id 24689, :coor [3 1], :result "15" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1326303898, :form-flow-id 24689, :coor [], :result "15", :outer-form? true :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1326303898, :form-flow-id 24689, :coor [2 1], :result "15" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1326303898, :form-flow-id 24689, :coor [], :result "15", :outer-form? true :timestamp 1}]]]

    (with-redefs [t/get-timestamp (constantly 1)
                  t/ws-send (fn [event] (swap! sent-events conj event))
                  rand-int (constantly 1)]

      (multi-arity-foo 5)

      (doseq [[et se] (map vector expected-traces @sent-events)]
        (is (= (event-data-without et :form-flow-id)
               (event-data-without se :form-flow-id))
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
        expected-traces [[:flow-storm/init-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :form "(defn err-foo [] (->> (range 10) (map (fn [i] (if (= i 2) (bad-fn) i))) doall))", :args-vec "[]", :fn-name "err-foo" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1], :result "[stripped-object]" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 1], :result "(0 1 2 3 4 5 6 7 8 9)" :timestamp 1}]
                         [:flow-storm/add-bind-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :timestamp 1, :coor [3 2 1], :symbol "i", :value "0"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1 2 1 1], :result "0" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1 2 1], :result "false" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1 2 3], :result "0" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1 2], :result "0" :timestamp 1}]
                         [:flow-storm/add-bind-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :timestamp 1, :coor [3 2 1], :symbol "i", :value "1"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1 2 1 1], :result "1" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1 2 1], :result "false" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1 2 3], :result "1" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1 2], :result "1" :timestamp 1}]
                         [:flow-storm/add-bind-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :timestamp 1, :coor [3 2 1], :symbol "i", :value "2"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1 2 1 1], :result "2" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1 2 1], :result "true" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1 2 2], :err #:error{:message error-msg} :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2 1 2], :err #:error{:message error-msg} :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3 2], :err #:error{:message error-msg} :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [3], :err #:error{:message error-msg} :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1423620308, :form-flow-id 45906, :coor [], :err #:error{:message error-msg}, :outer-form? true :timestamp 1}]]]
    (with-redefs [t/get-timestamp (constantly 1)
                  t/ws-send (fn [event] (swap! sent-events conj event))
                  rand-int (constantly 1)]
      (let [msg (try
                  (err-foo)
                  #?(:clj (catch Exception e (.getMessage e))
                     :cljs (catch :default e (.-message e))))])

      (doseq [[et se] (map vector expected-traces @sent-events)]

        (is (= (event-data-without et :form-flow-id)
               (cond-> se
                 true (event-data-without :form-flow-id)
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
    (with-redefs [t/get-timestamp (constantly 1)
                  t/ws-send (fn [event] (swap! sent-events conj event))]
      (zfoo 42 42)

      (is (-> @sent-events first second :fixed-flow-id-starter?)
          ":fixed-flow-id-starter? should be true on the first :flow-storm/init-trace ")

      (doseq [[_ {:keys [flow-id]}] @sent-events]
        (is (zero? flow-id) "A zero traced flow-id is not zero.")))))

(deftest case-test
  (let [sent-events (atom [])
        expected-traces [[:flow-storm/init-trace {:flow-id 1, :form-id 939228297, :form-flow-id 35800, :form "(case :val1 :val1 (+ 1 2) :val2 0)" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 939228297, :form-flow-id 35800, :coor [3], :result "3" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 939228297, :form-flow-id 35800, :coor [], :result "3" :timestamp 1}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 939228297, :form-flow-id 35800, :coor [], :result "3", :outer-form? true :timestamp 1}]]]

    (with-redefs [t/get-timestamp (constantly 1)
                  t/ws-send (fn [event] (swap! sent-events conj event))
                  rand-int (constantly 1)]

      ;; testing https://github.com/jpmonettas/flow-storm/issues/9
      #trace (case :val1
               :val1 (+ 1 2)
               :val2 0)

      (doseq [[et se] (map vector expected-traces @sent-events)]

        (is (= (event-data-without et :form-flow-id)
               (event-data-without se :form-flow-id))
            "A generated trace doesn't match with the expected trace")))))


;; trace/untrace var test can only be done in clojure since we can't change namespaces with in-ns in ClojureScript
#?(:clj
   (deftest trace-var-test
     (let [sent-events (atom [])]

       (with-redefs [t/get-timestamp (constantly 1)
                     t/ws-send (fn [event] (swap! sent-events conj event))
                     rand-int (constantly 1)]

         (testing "Instrumenting a var with trace-var works correctly"

           (fsa/trace-var clojure.java.shell/sh)
           
           (is (= (:exit (shell/sh "ls")) 0)
               "Instrumented clojure.java.shell/sh fn doesn't work correctly after trace-var")

           (is (= (count @sent-events) 47)
               "Calling clojure.java.shell/sh didn't generate the corret trace count"))

         (reset! sent-events [])

         (testing "Un-instrumenting a var with untrace-var works correctly"
           
           (fsa/untrace-var clojure.java.shell/sh)
           
           (is (= (:exit (shell/sh "ls")) 0)
               "We didn't return the clojure.java.shell/sh var to its original implementation after untrace-var")

           (is (empty? @sent-events)))))))

(deftest ref-tracing-test
  (let [sent-events (atom [])
        init-val {:name "foo" :age 37}
        expected-traces [[:flow-storm/ref-init-trace {:ref-id 1, :ref-name :person-state, :init-val (t/serialize-val init-val) :timestamp 1}]
                         [:flow-storm/ref-trace {:ref-id 1, :patch "[[[:age] :r 38]]" :timestamp 1}]
                         [:flow-storm/ref-trace {:ref-id 1, :patch "[[[:address] :+ \"montevideo/uruguay\"]]" :timestamp 1}]]]

    (with-redefs [t/get-timestamp (constantly 1)
                  t/ws-send (fn [event] (swap! sent-events conj event))
                  rand-int (constantly 1)]

      (testing "Tracing and untracing references"
        
        (let [person-state (atom (assoc init-val
                                        :a-big-value :big))] ;; this shouldn't be traced
          
          (t/trace-ref person-state {:ref-name :person-state
                                     :ignore-keys [:a-big-value]})

          (swap! person-state update :age inc)
          (swap! person-state assoc :address "montevideo/uruguay")
          (swap! person-state assoc :a-big-value :even-bigger) ;; this shouldn't be traced
          
          (t/untrace-ref person-state)

          (swap! person-state update :age inc)
          
          (doseq [[et se] (map vector expected-traces @sent-events)]
            (is (-> se second :ref-id number?)
                "A generated ref-trace doesn't contain ref-id")

            (is (=
                 (pr-str (event-data-without et :ref-id))
                 (pr-str (event-data-without se :ref-id)))
                "A generated trace doesn't match with the expected trace"))

          (testing "We can recover the value from traces"
            (let [traced-val (reduce (fn [r [_ {:keys [patch]}]]
                                       (edit.core/patch r (edit.edit/edits->script (read-str patch))))
                                     init-val
                                     (rest expected-traces))]
              (is (= traced-val
                     {:name "foo"
                      :age 38
                      :address "montevideo/uruguay"})
                  
                  "The recovered value after applying patches is wrong"))))))))

#?(:clj
   (deftest tap-tracing-test
     (let [sent-events (atom [])
           expected-traces [[:flow-storm/tap-trace {:tap-id 1, :tap-name "test-tap", :value "[1 2 3]" :timestamp 1}]
                            [:flow-storm/tap-trace {:tap-id 1, :tap-name "test-tap", :value "{:value 42}" :timestamp 1}]
                            [:flow-storm/tap-trace {:tap-id 1, :tap-name "test-tap", :value "77" :timestamp 1}]]]
       (with-redefs [t/get-timestamp (constantly 1)
                     t/ws-send (fn [event] (swap! sent-events conj event))]
         (t/init-tap 1 "test-tap")

         (tap> [1 2 3])
         (tap> {:value 42})
         (tap> 77)

         (Thread/sleep 1000)
         
         (is (= expected-traces @sent-events)
             "Expected tap traces aren't the same as expected traces."))))

   ;; The functionality is working in CLJS but the with-redefs doesn't work
   ;; when ws-send event is being called by the tap fn.
   ;; Don't know why it works in CLJ, since the thread that is executing taps
   ;; isn't probably the same that the one we are redefining for.
   ;;
   ;; :cljs
   ;; (deftest tap-tracing-test
   ;;   (async done
   ;;          (let [sent-events (atom [])
   ;;                expected-traces [[:flow-storm/tap-trace {:tap-id 1, :tap-name "test-tap", :value [1 2 3]}]
   ;;                                 [:flow-storm/tap-trace {:tap-id 1, :tap-name "test-tap", :value {:value 42}}]
   ;;                                 [:flow-storm/tap-trace {:tap-id 1, :tap-name "test-tap", :value 77}]]]
   ;;            (with-redefs [t/ws-send (fn [event]
   ;;                                      (swap! sent-events conj event))]
   ;;              (t/init-tap 1 "test-tap")

   ;;              (tap> [1 2 3])
   ;;              (tap> {:value 42})
   ;;              (tap> 77)

   ;;              (js/setTimeout (fn []
   ;;                               (is (= expected-traces @sent-events)
   ;;                                   "Expected tap traces aren't the same as expected traces.")
   ;;                               (done))
   ;;                             1000)))))
   )
