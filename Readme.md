# flow-storm

Tracing companion library for the [flow-storm-debugger](https://github.com/jpmonettas/flow-storm-debugger) (A Clojure and ClojureScript debugger)

Use this library to instrument your code.

Tested on jvm, browser, nodejs, react-native.

[![Clojars Project](https://img.shields.io/clojars/v/jpmonettas/flow-storm.svg)](https://clojars.org/jpmonettas/flow-storm)

Big thanks to the [Cider](https://github.com/clojure-emacs/cider-nrepl) team for `cider.nrepl.middleware.util.instrument` since this code started as a fork of it.

## Step 1 (Running the debugger)

In a terminal run a [flow-storm-debugger](https://github.com/jpmonettas/flow-storm-debugger) instance.

```bash
clj -Sdeps '{:deps {jpmonettas/flow-storm-debugger {:mvn/version "0.4.0"}}}' -m flow-storm-debugger.server
```

This will run the debugger. For instructions on using the debugger see [flow-storm-debugger](https://github.com/jpmonettas/flow-storm-debugger).

One instance of the debugger is enough for all you Clojure and ClojureScript projects.

## Step 2 (Instrument your code)

Now add this library to your application dependencies (deps.edn, project.clj, shadow-cljs.edn, etc).

Simple repl example :

```bash
clj -Sdeps '{:deps {jpmonettas/flow-storm {:mvn/version "0.4.0"}}}'
```

```clojure
(require '[flow-storm.api :as fs-api])

;; Add this to your application start
;; or fire it once if you are working at the repl
;; it will connect to flow-storm-debugger via a websocket
(fs-api/connect)

;; And start tracing whatever you are interested in
;; by adding a #trace reader tag.
;; You can trace a entire function, or a single sub form.

#trace
(defn foo [a b]
  (+ a b))

#trace
(defn bar []
  (let [a 10]
	(->> (range (foo a a))
		 (map inc)
		 (filter odd?)
		 (reduce +))))

(bar)
```

Everytime a traced **flow** executes, it will trace the execution in the debugger.

You can use **#ztrace** instead of **#trace** to make the flow-id always be 0. This is useful
for incrementaly trying things at the repl, and stopping the debugger from creating a different flow tab each time
you run the expression.

## Connecting to a debugger remotely

If you need to connect to a debugger instance not running in the same device as your debugged procees you can optionally provide
`flow-storm.api/connect` the following options:

- protocol Can be `:http` or `:https` (defaults to :http)
- host (defaults to "localhost")
- port (defaults to 7722)

## Instrumenting library code with trace-var

Tracing library code in Clojure is pretty straight forward, just use `flow-storm.api/trace-var`.

Let's say you want to trace the clojure.core/odd? function. You would have to :

```clojure
user> (fsa/trace-var clojure.core/odd?)
user> (odd? 5) ;; This will generate traces
true
user> (fsa/untrace-var clojure.core/odd?)
user> (odd? 5) ;; This will be the original function again
true

```

Currently tracing library code in ClojureScript is a little more involved than I like, but couldn't figure out a better way yet.

Let's say you want to trace the cljs.core/odd? function. You would have to :

```clojure
cljs.user> (in-ns 'cljs.core)
cljs.core> (flow-storm.api/trace-var odd?) ;; You need to run trace-var inside the namespace where the var you are trying to instrument is defined
cljs.core> (in-ns 'cljs.user)
cljs.user> (odd? 5) ;; This will generate traces
```

**Important Note: be carefull to not instrument a function that is being continuously called by your environment**

For example if you try to instrument `cljs.core/map` which is constantly being called by shadow-cljs repl it will generate a bunch of traces you are probably not interested in.

Repeat the same procedure but with `flow-storm.api/untrace-var` to replace the var with the original function version.

## Tracing references

You can trace references (like atoms) by using `flow-storm.api/trace-ref` like :

```clojure
user> (fsa/trace-ref re-frame.db/app-db {:ref-name "re-frame-state"})
```
## Notes

**On node js you need the npm websocket library for the library to work!**.

For instrumenting remote code (like in react-native) use :

```clojure
(fs-api/connect {:host "192.168.1.8" :port 7722})
```
