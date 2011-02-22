(ns user
  (:use [clojure.contrib.repl-utils :only (show expression-info)]))

(set! clojure.core/*print-length* 23)

(defmacro ns-reload! [ns]
  `(do
     (if (find-ns '~ns)
       (doseq [s# (keys (ns-publics '~ns))] (ns-unmap '~ns s#)))
     (require :reload-all '~ns)))

(defn dbg-1* [expr-prn expr-dbg]
  `(let [a# ~expr-dbg]
     (println '~expr-prn "->" a#)
     a#))

(defn dbg* [expr]
  (if (and (seq? expr) (not= (first expr) 'quote))
    (let [expr (macroexpand expr)]
      (dbg-1* expr (cons (first expr) (map dbg* (rest expr)))))
    expr))

(defmacro dbg-1 [expr]
  (dbg-1* expr expr))

(defmacro dbg [expr]
  (dbg* expr))

(defn expression-classname [expr]
  (when-let [cls (:class (expression-info expr))]
    (.getName cls)))

(defn expression-javadoc [expr]
  (when-let [cls (:class (expression-info expr))]
    (javadoc cls)))
