# flow-storm

**WIP !!!**

Tracing companion library for the [flow-storm-debugger](https://github.com/jpmonettas/flow-storm-debugger) (A experimental Clojure and ClojureScript debugger)

Use this library to instrument your code.

Tested on jvm, browser, nodejs, react-native.

[![Clojars Project](https://img.shields.io/clojars/v/jpmonettas/flow-storm.svg)](https://clojars.org/jpmonettas/flow-storm)

## Before starting your day

In a terminal run a [flow-storm-debugger](https://github.com/jpmonettas/flow-storm-debugger) instance.

```bash
clj -Sdeps '{:deps {jpmonettas/flow-storm-debugger {:mvn/version "0.2.0"}}}' -m flow-storm-debugger.server
```
And point your browser to http://localhost:7722

Now in you application add this library to your dependencies (deps.edn, project.clj, shadow-cljs.edn, etc)

Simple repl example :

```bash
clj -Sdeps '{:deps {jpmonettas/flow-storm {:mvn/version "0.2.1"}}}'
```

```clojure
(require '[flow-storm.api :as fs-api :refer [trace]])

;; Add this to your application start
;; or fire it once if you are working at the repl
;; it will connect to flow-storm-debugger via a websocket
(fs-api/connect)

;; And start tracing whatever you are interested in

(trace
 (defn foo [a b]
   (+ a b)))

(trace
 (defn bar []
   (let [a 10]
     (->> (range (foo a a))
          (map inc)
          (filter odd?)
          (reduce +)))))

(bar)
```

Everytime a traced "flow" executes, it will trace the execution in the debugger.

## Notes

On node js you need the npm websocket library for the library to work.

For instrumenting remote code (like in react-native) use :

```clojure
(fs-api/connect {:host "192.168.1.8" :port 7722})
```
