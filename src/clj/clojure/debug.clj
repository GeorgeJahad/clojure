(ns clojure.debug
  (:require clojure.main))


;; dynamic bindings don't seem to work at compile time so I had to use
;; alter-var-root.  with-alter-var-root doesn't work because it is
;; nested in one big let block and compiler granularity requires
;; it to be in a "do" block

(defn alter-helper [a b] b)

(defmacro defn-debug
  "Like defn, but turns on lexical frame creation"
  [fn-name & defn-stuff]
  `(do
     (alter-var-root #'clojure.core/*create-lexical-frames* alter-helper true)
     (try
      (defn ~fn-name ~@defn-stuff)
      (finally
       (alter-var-root #'clojure.core/*create-lexical-frames*
                       alter-helper false)))))

(defmacro debug-bindings
  "Turns on lexical frame creation"
  [& body]
  `(do
     (alter-var-root #'clojure.core/*create-lexical-frames* alter-helper true)
     (try
      ~@body
      (finally
       (alter-var-root #'clojure.core/*create-lexical-frames*
                       alter-helper false)))))

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

(def context-var nil)

(defn eval-with-context-fn [context form]
  (binding [context-var context]
    (let [ret (eval `(eval-with-context context-var ~form))]
      ret)))

(defmacro debug-repl [context]
  `(clojure.main/repl :prompt #(print "dr => ")
                      :eval (partial eval-with-context-fn ~context)))