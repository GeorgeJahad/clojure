(ns clojure.debug)

(defmacro defn-debug
  "Just like defn, but turns on lexical frame creation"
  [fn-name & defn-stuff]
  `(do
     ;;?? alter?
     (alter-var-root #'clojure.core/*create-lexical-frames*
                     (fn [~'c ~'d] ~'d)  true)
     (defn ~fn-name ~@defn-stuff)
     (alter-var-root #'clojure.core/*create-lexical-frames*
                     (fn [~'c ~'d] ~'d) false)
     nil))

(defmacro get-context [context]
  `(def ~context (get-thread-bindings)))

(defn make-let-bindings [lex-bindings]
  (apply concat
         (for [[sym val] lex-bindings]
           [sym val])))

(defmacro eval-with-context [context form]
  (do
    ;;eval??
    (push-thread-bindings (eval context))
    (try
     (let [lex-bindings (apply merge {} clojure.core/*lexical-frames*)]
       `(do
          (push-thread-bindings ~context)
          (try
           (eval
            (let [~@(make-let-bindings lex-bindings)]
              ~form))
           (finally (pop-thread-bindings)))))
     (finally (pop-thread-bindings)))))
