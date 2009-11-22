(ns clojure.debug)

(defmacro defn-debug
  "Just like defn, but memoizes the function using clojure.core/memoize"
  [fn-name & defn-stuff]
  `(binding [clojure.core/*create-lexical-frames* true]
     (defn ~fn-name ~@defn-stuff)))

(defmacro get-context [context]
  `(def ~context (get-thread-bindings)))

(defn make-let-bindings []
  (apply concat
         (for [[sym val] (apply merge {} clojure.core/*lexical-frames*)]
           [sym val])))

(defmacro eval-with-context [context form]
  `(do
     (push-thread-bindings ~context)
     (try
      (eval
       (let [~@(make-let-bindings)]
         ~form))
      (finally (pop-thread-bindings)))))