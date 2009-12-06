;; Copyright (c) Rich Hickey All rights reserved. The use and
;; distribution terms for this software are covered by the Eclipse Public
;; License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found
;; in the file epl-v10.html at the root of this distribution. By using this
;; software in any fashion, you are agreeing to be bound by the terms of
;; this license. You must not remove this notice, or any other, from this
;; software.

;; Contributed by George Jahad
;; This code properly belongs in clojure.contrib, but I since I had to
;; hack the compiler anyway I decided to put it here for now, just to
;; minimize the number of patched repos users need.

(ns clojure.debug
  (:require clojure.main))

(defstruct context-struct :bindings :lexical-frames)

(def context-cmd '(struct context-struct
                          (get-thread-bindings) *lexical-frames*))

(defmacro get-context [context] `(def ~context ~context-cmd))

(defn make-let-bindings [lex-bindings frames]
  (mapcat #(vector % `(~lex-bindings '~%))
          (keys (into {} frames))))

(defmacro eval-with-context [context form]
  (let  [lex-bindings (gensym)]
    ;; don't use "do" with push-thread-bindings, because "do" causes
    ;;  each sub form to be eval'd separately, with
    ;;  pop-thread-bindings after each, causing unbalanced push/pops.
    ;;  Use "(let [])" instead, as that causes all the sub-forms to be
    ;;  eval'd together with no itermediate pops
    `(let []
       (push-thread-bindings (:bindings ~context))
       (try
        (eval
         '(let [~lex-bindings (into {} (:lexical-frames ~context))]
            (let [~@(make-let-bindings
                     lex-bindings
                     (:lexical-frames (var-get (resolve context))))]
              ~form)))
        (finally (pop-thread-bindings))))))

(defn eval-with-context-fn [context form]
  (eval `(eval-with-context ~context ~form)))

(def debug-repl-context nil)

(defmacro debug-repl
  ([]
     `(binding [debug-repl-context ~context-cmd]
        (debug-repl debug-repl-context)))
  ([context]
     `(clojure.main/repl :prompt #(print "dr => ")
                         :eval (partial eval-with-context-fn '~context))))

(defmacro with-lexical-frames
  "Turns on lexical frame creation"
  [& body]
  `(binding [*create-lexical-frames* true]
     (eval '~@body)))

(defmacro defn-debug
  [fn-name & defn-stuff]
  `(with-lexical-frames
     (defn ~fn-name ~@defn-stuff)))

(defmacro deftest-debug
  [test-name & deftest-stuff]
  `(with-lexical-frames
     (deftest ~test-name ~@deftest-stuff)))

(defmacro defmacro-debug
  [macro-name & defmacro-stuff]
  `(with-lexical-frames
     (defmacro ~macro-name ~@defmacro-stuff)))
