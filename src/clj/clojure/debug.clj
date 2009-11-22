(ns clojure.debug
  (:require clojure.main))

(defmacro defn-debug
  "Like defn, but turns on lexical frame creation"
  [fn-name & defn-stuff]
  `(do
     (push-thread-bindings {#'clojure.core/*create-lexical-frames* true})
     (try
      (defn ~fn-name ~@defn-stuff)
      (finally (pop-thread-bindings)))))

(defmacro get-context [context]
  `(def ~context (get-thread-bindings)))

(defn make-let-bindings []
  (apply concat
         (for [[sym val] (apply merge {} clojure.core/*lexical-frames*)]
           [sym val])))

(defmacro eval-with-context [context form]
  (do
    ;;eval??
    (push-thread-bindings (eval context))
    (try
     `(do
        (push-thread-bindings ~context)
        (try
         (eval
          (let [~@(make-let-bindings)]
            ~form))
         (finally (pop-thread-bindings))))
     (finally (pop-thread-bindings)))))

(defmacro gen-eval-with-context [context]
  (do
    ;;eval??
    (push-thread-bindings (eval context))
    (try
     `(do
        (push-thread-bindings ~context)
        (try
         (fn [form#]
           (eval
            (let [~@(make-let-bindings)]
              form#)))
         (finally (pop-thread-bindings))))
     (finally (pop-thread-bindings)))))

(def context-var nil)

(defn eval-with-context-fn [context form]
  (do
    (alter-var-root #'clojure.debug/context-var
                    (fn [c d] d)  context)
    (let [ret (eval (eval `(eval-with-context context-var ~form)))]
      (alter-var-root #'clojure.debug/context-var
                      (fn [c d] d)  nil)
      ret)))

(defmacro debug-repl [context]
  `(clojure.main/repl :prompt #(println "hi george") :eval (partial eval-with-context-fn ~context)))