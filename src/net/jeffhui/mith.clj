(ns net.jeffhui.mith
  (:require [clojure.string :as string]))

(declare html)
(defn compile-html [form]
 (cond
   (vector? form)
   (let [sec (second form)
         attrs (when (map? sec) sec)
         children (if (map? sec)
                    (rest (rest form))
                    (rest form))]
     `(net.jeffhui.mith/element ~(if (keyword? (first form))
                                   (name (first form))
                                   (first form))
                                (clojure.core/clj->js ~(merge attrs
                                                              {:class (string/join " " (let [cls (:class attrs)]
                                                                                         (if (string? cls) [cls] cls)))}))
                                (into-array ~(mapv (fn [f] (compile-html f)) children))))

   (list? form)
   (case (name (first form))
     "if" `(if ~(second form) ~(compile-html (nth form 2)) ~(compile-html (nth form 3)))
     "if-not" `(if-not ~(second form) ~(compile-html (nth form 2)) ~(compile-html (nth form 3)))
     "if-some" `(if-some ~(second form) ~(compile-html (nth form 2)) ~(compile-html (nth form 3)))
     "when" `(when ~@(butlast form) ~(compile-html (last form)))
     "when-not" `(when-not ~@(butlast form) ~(compile-html (last form)))
     "when-some" `(when-some ~@(butlast form) ~(compile-html (last form)))
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
     "let" `(let ~@(butlast form) ~(compile-html (last form)))
     "let*" `(let* ~@(butlast form) ~(compile-html (last form)))
     "letfn" `(letfn ~@(butlast form) ~(compile-html (last form)))
     "letfn*" `(letfn* ~@(butlast form) ~(compile-html (last form)))
     "for" `(for ~(second form) ~(compile-html (nth form 2)))
     "do" (if (= 1 (count form))
           (compile-html (first form))
           `(do ~@(butlast form) ~(compile-html (last form))))
     `(net.jeffhui.mith/interpret-element ~form))

   (string? form) form
   (number? form) form

   :else `(net.jeffhui.mith/interpret-element ~form)))

(defmacro html [form]
  (cond
    (vector? form)
    (compile-html form)

    (keyword? form)
    (name form)

    :else
    form))
