(ns net.jeffhui.mith
  (:require [clojure.string :as string]))

(defn compile-html [form]
  (cond
    (and (vector? form)
         (or
          (keyword? (first form))
          (string? (first form))
          (symbol? (first form))))
    (let [sec (second form)
          attrs (when (map? sec) sec)
          children (not-empty
                    (if (map? sec)
                      (rest (rest form))
                      (rest form)))]
      `(net.jeffhui.mith/element ~(if (keyword? (first form))
                                    (name (first form))
                                    (first form))
                                 ~(when attrs
                                    `(clojure.core/clj->js ~(merge attrs
                                                                   (when (seq (:class attrs))
                                                                     {:class (string/join " " (let [cls (:class attrs)]
                                                                                                (if (string? cls) [cls] cls)))}))))

                                 ~(when children
                                    `(into-array ~(mapv (fn [f] (compile-html f)) children)))))

    (and (seq? form) (symbol? (first form)))
    (case (name (first form))
      "if" `(if ~(second form) ~(compile-html (nth form 2)) ~(compile-html (nth form 3)))
      "if-not" `(if-not ~(second form) ~(compile-html (nth form 2)) ~(compile-html (nth form 3)))
      "if-some" `(if-some ~(second form) ~(compile-html (nth form 2)) ~(compile-html (nth form 3)))
      "when" `(when ~@(butlast (rest form)) ~(compile-html (last form)))
      "when-not" `(when-not ~@(butlast (rest form)) ~(compile-html (last form)))
      "when-some" `(when-some ~@(butlast (rest form)) ~(compile-html (last form)))
      "case" (let [[_ value & clauses] form]
               `(case ~value
                  ~@(mapcat (fn [[condition expr]]
                              (if expr
                                [condition (compile-html expr)]
                                [(compile-html condition)]))
                            (partition-all 2 clauses))))
      "condp" (let [[_ f value & clauses] form]
                `(condp ~f ~value
                   ~@(mapcat (fn [[condition expr]]
                               (if expr
                                 [condition (compile-html expr)]
                                 [(compile-html condition)]))
                             (partition-all 2 clauses))))
      "cond" (let [clauses (mapcat (fn [[condition expr]]
                                     (if expr
                                       [condition (compile-html expr)]
                                       [(compile-html condition)]))
                                   (partition-all 2 (rest form)))]
               `(cond ~@clauses))
      "let" `(let ~@(butlast (rest form)) ~(compile-html (last form)))
      "let*" `(let* ~@(butlast (rest form)) ~(compile-html (last form)))
      "letfn" `(letfn ~@(butlast (rest form)) ~(compile-html (last form)))
      "letfn*" `(letfn* ~@(butlast (rest form)) ~(compile-html (last form)))
      "for" `(for ~(second form) ~(compile-html (nth form 2)))
      "do" (if (= 1 (count form))
             (compile-html (first form))
             `(do ~@(butlast (rest form)) ~(compile-html (last form))))
      `(net.jeffhui.mith/interpret-element ~form))

    (or
     (string? form)
     (number? form)
     (map? form)
     (vector? form)
     (set? form)
     (seq? form))
    form

    :else `(net.jeffhui.mith/interpret-element ~form)))

(defmacro html [form]
  (compile-html form))
