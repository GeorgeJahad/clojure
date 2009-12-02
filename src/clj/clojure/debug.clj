;; Copyright (c) Rich Hickey All rights reserved. The use and
;; distribution terms for this software are covered by the Eclipse Public
;; License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found
;; in the file epl-v10.html at the root of this distribution. By using this
;; software in any fashion, you are agreeing to be bound by the terms of
;; this license. You must not remove this notice, or any other, from this
;; software.

;; Contributed by George Jahad

(ns clojure.debug
  (:require clojure.main))

;; dynamic bindings don't seem to work at compile time so I had to use
;; alter-var-root.  with-alter-var-root doesn't work because it is
;; nested in one big let block and compiler granularity requires
;; it to be in a "do" block

(defn alter-helper [a b] b)

(defmacro with-lexical-frames
  "Turns on lexical frame creation"
  [& body]
  `(do
     (alter-var-root #'clojure.core/*create-lexical-frames* alter-helper true)
     (try
      ~@body
      (finally
       (alter-var-root #'clojure.core/*create-lexical-frames*
                       alter-helper false)))))

(defmacro defn-debug
  [fn-name & defn-stuff]
  `(with-lexical-frames
     (defn ~fn-name ~@defn-stuff)))

(defmacro deftest-debug
  [test-name & deftest-stuff]
  `(with-lexical-frames
     (deftest ~test-name ~@deftest-stuff)))

(defmacro get-context [context]
  `(def ~context (get-thread-bindings)))

(defn make-let-bindings [lex-bindings]
  (mapcat #(vector % `(~lex-bindings '~%))
          (keys (into {} clojure.core/*lexical-frames*))))

(defmacro eval-with-context [context form]
  (do
    ;;eval??
    (push-thread-bindings (eval context))
    (try
     (let  [lex-bindings (gensym)]
       ;; don't use "do" with push-thread-bindings, because "do" causes
       ;;  each sub form to be eval'd separately, with
       ;;  pop-thread-bindings after each, causing unbalanced push/pops.
       ;;  Use "(let [])" instead, as that causes all the sub-forms to be
       ;;  eval'd together with no itermediate pops
       `(let []
          (push-thread-bindings ~context)
          (try
           (eval
            '(let [~lex-bindings (into {} clojure.core/*lexical-frames*)]
               (let [~@(make-let-bindings lex-bindings)]
                 ~form)))
           (finally (pop-thread-bindings)))))
     (finally (pop-thread-bindings)))))

(defn eval-with-context-fn [context form]
  (eval `(eval-with-context ~context ~form)))

(defmacro debug-repl [context]
  `(clojure.main/repl :prompt #(print "dr => ")
                      :eval (partial eval-with-context-fn '~context)))

