;; This isn't being used, added to experiment with mranderson for
;; inlining deps, but it is throwing :
;;
;; ...
;; prefixing imports in clojure files in 'target/srcdeps/taoensso' ...
;; prefixing imports: done
;; java.io.FileNotFoundException: target/srcdeps/taoensso/timbre/appenders/3rd_party/appenders/newrelic.clj (No such file or directory)
;; ...

(defproject jpmonettas/flow-storm "0.2.1"
  :url "https://github.com/jpmonettas/flow-storm"
  :dependencies [^:inline-dep [org.clojure/clojurescript "1.10.758"]
                 ^:inline-dep [com.taoensso/sente "1.16.0-alpha2"]]

  :plugins [[thomasa/mranderson "0.5.1"]]
  :test-paths ["test"])
