# flow-storm

**WIP !!!**

Tracing library for the [flow-storm-debugger](https://github.com/jpmonettas/flow-storm-debugger) (A experimental Clojure and ClojureScript debugger)

Tested on jvm, browser, nodejs, react-native.

[![Clojars Project](https://img.shields.io/clojars/v/flow-storm.svg)](https://clojars.org/flow-storm)

## Before starting your day

In a terminal run a [flow-storm-debugger](https://github.com/jpmonettas/flow-storm-debugger) instance.

```bash
clj -Sdeps '{:deps {flow-storm-debugger {:mvn/version "0.1.0"}}}' -m flow-storm-debugger.server
```
And point your browser to http://localhost:7722

Now in you application:
    - add this library to your dependencies (deps.edn, project.clj, shadow-cljs.edn, etc)

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

```

Everytime a traced "flow" executes, it will trace the execution in the debugger.

## Notes

On node js you need the npm websocket library for the library to work.
