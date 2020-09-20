(ns flow-storm.all-test
  (:require [clojure.pprint :as pp]
            [flow-storm.api :as fsa]
            [clojure.test :refer [deftest is testing]]
            [flow-storm.tracer :as t]
            [clojure.string :as str]))

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
        clean-clj-fn-print (fn [fp]
                             (if (str/starts-with? fp "#object")
                               (str/replace fp #"\s.*[a-zA-Z0-9\"\.\$]" "")
                               fp))
        expected-traces [[:flow-storm/init-trace {:flow-id 1, :form-id 1267089144, :form "(defn bar [] (let [a 10] (->> (range (foo a a)) (map inc) (filter odd?) (reduce +))))"}]
                         [:flow-storm/add-bind-trace {:flow-id 1, :form-id 1267089144, :coor [3], :symbol "a", :value "10"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3 2 4 1], :result #?(:cljs "#object[cljs$core$_PLUS_]" :clj "#object[clojure.core$_PLUS_]")}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3 2 3 1], :result #?(:cljs "#object[cljs$core$odd_QMARK_]" :clj "#object[clojure.core$odd_QMARK_]")}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3 2 2 1], :result #?(:cljs "#object[cljs$core$inc]" :clj "#object[clojure.core$inc]")}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3 2 1 1 1], :result "10"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id 1267089144, :coor [3 2 1 1 2], :result "10"}]
                         [:flow-storm/init-trace {:flow-id 1, :form-id -828991137, :form "(defn foo [a b] (+ a b (or 2 1)))"}]
                         [:flow-storm/add-bind-trace {:flow-id 1, :form-id -828991137, :coor nil, :symbol "a", :value "10"}]
                         [:flow-storm/add-bind-trace {:flow-id 1, :form-id -828991137, :coor nil, :symbol "b", :value "10"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id -828991137, :coor [3 1], :result "10"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id -828991137, :coor [3 2], :result "10"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id -828991137, :coor [3 3], :result "2"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id -828991137, :coor [3], :result "22"}]
                         [:flow-storm/add-trace {:flow-id 1, :form-id -828991137, :coor [], :result "22", :outer-form? true}]
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
        (is (= et (cond-> se
                    (contains? (second se) :result) (update-in [1 :result] clean-clj-fn-print)))
            "A generated trace doesn't match with the expected trace")))))
