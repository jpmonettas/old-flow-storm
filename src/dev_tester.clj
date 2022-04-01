(ns dev-tester)

;;;;;;;;;;;;;;;;;;;;;;;
;; Some testing code ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defn factorial [n]
  (if (zero? n)
    1
    (* n (factorial (dec n)))))

(defmulti do-it type)

(defmethod do-it java.lang.Long
  [l]
  (factorial l))

(defmethod do-it java.lang.String
  [s]
  (count s))

(defprotocol Adder
  (add [x]))

(defprotocol Suber
  (sub [x]))

(extend-protocol Adder

  java.lang.Long
  (add [l] (+ l 5)))

(extend-type java.lang.Long

  Suber
  (sub [l] (- l 42)))

(defn boo [xs]
  (let [a 25
        b (+ a 4)
        c (+ a b 7)]
    (-> (reduce + (map do-it xs))
       add
       sub)))
