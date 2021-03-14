(ns net.jeffhui.mith-test
  (:require [net.jeffhui.mith :as m]
            [clojure.test :refer [deftest are testing is]]))

(def e `m/element)
(def clj->js `clojure.core/clj->js)
(def interp `m/interpret-element)

(declare foo bar)

(deftest test-compilation-of-html
  (testing "Basic element transformations"
    (are [input expected] (= (m/compile-html input) expected)
      [:div] `(~e "div" nil nil)
      [:div#id] `(~e "div#id" nil nil)
      [:div#id.class1.class2] `(~e "div#id.class1.class2" nil nil)
      [:div.class] `(~e "div.class" nil nil)

      [:h1 "hello"] `(~e "h1" nil (into-array ["hello"]))
      [:h1 "hello" "world"] `(~e "h1" nil (into-array ["hello" "world"]))
      [:h1 {:class "foo"}] `(~e "h1" (~clj->js {:class "foo"}) nil)

      [:p {:style "background: red"} "whoa"] `(~e "p" (~clj->js {:style "background: red"}) (into-array ["whoa"]))
      [:p.c {:class ["a" "b"]} "whoa"] `(~e "p.c" (~clj->js {:class "a b"}) (into-array ["whoa"]))))

  (testing "Nested element transformations"
    (are [input expected] (= (m/compile-html input) expected)
      [:div [:h1 "foo"]] `(~e "div" nil (into-array [(~e "h1" nil (into-array ["foo"]))]))
      [:div [:h2 "foo"] [:h3 "bar"]] `(~e "div" nil (into-array [(~e "h2" nil (into-array ["foo"]))
                                                                 (~e "h3" nil (into-array ["bar"]))]))))

  (testing "Strings and numbers are left as is"
    (is (= (m/compile-html "hello") "hello"))
    (is (= (m/compile-html 1) 1)))

  (testing "Unknown values are pass through an interpreter"
    (are [input expected] (= (m/compile-html input) expected)
      #{:a :b :c} #{:a :b :c}
      '(:a :b :c) '(:a :b :c)
      {:foo :bar} {:foo :bar}
      [2] [2]
      `[foo] `(~e foo nil nil)))

  (testing "let form"
    (are [input expected] (= (m/compile-html input) expected)
      `(let [x (foo 2)] [:div x])
      `(let [x (foo 2)] (~e "div" nil (into-array [(~interp x)])))))

  (testing "let* form"
    (are [input expected] (= (m/compile-html input) expected)
      `(let* [x (foo 2)] [:div x])
      `(let* [x (foo 2)] (~e "div" nil (into-array [(~interp x)])))))

  (testing "letfn form"
    (are [input expected] (= (m/compile-html input) expected)
      `(letfn [x (foo [] 2)] [:div (x)])
      `(letfn [x (foo [] 2)] (~e "div" nil (into-array [(~interp (x))])))))

  (testing "letfn* form"
    (are [input expected] (= (m/compile-html input) expected)
      `(letfn* [x (foo [] 2)] [:div (x)])
      `(letfn* [x (foo [] 2)] (~e "div" nil (into-array [(~interp (x))])))))

  (testing "for form"
    (are [input expected] (= (m/compile-html input) expected)
      `(for [x (range 10)] [:div x])
      `(for [x (range 10)] (~e "div" nil (into-array [(~interp x)])))))

  (testing "do form"
    (are [input expected] (= (m/compile-html input) expected)
      `(do [:div foo] [:div bar])
      `(do [:div foo] (~e "div" nil (into-array [(~interp bar)])))))

  (testing "if form"
    (are [input expected] (= (m/compile-html input) expected)
      `(if (foo 2) [:div 1] [:div 2])
      `(if (foo 2)
         (~e "div" nil (into-array [1]))
         (~e "div" nil (into-array [2])))

      `(if (foo 2) [:div foo] [:div bar])
      `(if (foo 2)
         (~e "div" nil (into-array [(~interp foo)]))
         (~e "div" nil (into-array [(~interp bar)])))))

  (testing "if-not form"
    (are [input expected] (= (m/compile-html input) expected)
      `(if-not (foo 2) [:div 1] [:div 2])
      `(if-not (foo 2)
         (~e "div" nil (into-array [1]))
         (~e "div" nil (into-array [2])))

      `(if-not (foo 2) [:div foo] [:div bar])
      `(if-not (foo 2)
         (~e "div" nil (into-array [(~interp foo)]))
         (~e "div" nil (into-array [(~interp bar)])))))

  (testing "if-some form"
    (are [input expected] (= (m/compile-html input) expected)
      `(if-some (foo 2) [:div 1] [:div 2])
      `(if-some (foo 2)
         (~e "div" nil (into-array [1]))
         (~e "div" nil (into-array [2])))

      `(if-some (foo 2) [:div foo] [:div bar])
      `(if-some (foo 2)
         (~e "div" nil (into-array [(~interp foo)]))
         (~e "div" nil (into-array [(~interp bar)])))))

  (testing "when form"
    (are [input expected] (= (m/compile-html input) expected)
      `(when (foo 2) [:div 1] [:div 2])
      `(when (foo 2)
         [:div 1]
         (~e "div" nil (into-array [2])))

      `(when (foo 2) [:div foo] [:div bar])
      `(when (foo 2)
         [:div foo]
         (~e "div" nil (into-array [(~interp bar)])))))

  (testing "when-not form"
    (are [input expected] (= (m/compile-html input) expected)
      `(when-not (foo 2) [:div 1] [:div 2])
      `(when-not (foo 2)
         [:div 1]
         (~e "div" nil (into-array [2])))

      `(when-not (foo 2) [:div foo] [:div bar])
      `(when-not (foo 2)
         [:div foo]
         (~e "div" nil (into-array [(~interp bar)])))))

  (testing "when-some form"
    (are [input expected] (= (m/compile-html input) expected)
      `(when-some (foo 2) [:div 1] [:div 2])
      `(when-some (foo 2)
         [:div 1]
         (~e "div" nil (into-array [2])))

      `(when-some (foo 2) [:div foo] [:div bar])
      `(when-some (foo 2)
         [:div foo]
         (~e "div" nil (into-array [(~interp bar)])))))

  (testing "case form"
    (are [input expected] (= (m/compile-html input) expected)
      `(case (foo 2) 1 [:div 1] 2 [:div 2] [:div 3])
      `(case (foo 2)
         1 (~e "div" nil (into-array [1]))
         2 (~e "div" nil (into-array [2]))
         (~e "div" nil (into-array [3])))

      `(case (foo 2) 1 [:div foo] 2 [:div bar] [:div 4])
      `(case (foo 2)
         1 (~e "div" nil (into-array [(~interp foo)]))
         2 (~e "div" nil (into-array [(~interp bar)]))
         (~e "div" nil (into-array [4])))))

  (testing "condp form"
    (are [input expected] (= (m/compile-html input) expected)
      `(condp = (foo 2) 1 [:div 1] 2 [:div 2] [:div 3])
      `(condp = (foo 2)
         1 (~e "div" nil (into-array [1]))
         2 (~e "div" nil (into-array [2]))
         (~e "div" nil (into-array [3])))

      `(condp = (foo 2) 1 [:div foo] 2 [:div bar] [:div 4])
      `(condp = (foo 2)
         1 (~e "div" nil (into-array [(~interp foo)]))
         2 (~e "div" nil (into-array [(~interp bar)]))
         (~e "div" nil (into-array [4])))))

  (testing "cond form"
    (are [input expected] (= (m/compile-html input) expected)
      `(cond
         (= (foo 2) 1) [:div 1]
         (= (foo 2) 2) [:div 2]
         :else [:div 3])
      `(cond
         (= (foo 2) 1) (~e "div" nil (into-array [1]))
         (= (foo 2) 2) (~e "div" nil (into-array [2]))
         :else (~e "div" nil (into-array [3])))

      `(cond
         (= (foo 2) 1) [:div foo]
         (= (foo 2) 2) [:div bar]
         :else [:div 4])
      `(cond
         (= (foo 2) 1) (~e "div" nil (into-array [(~interp foo)]))
         (= (foo 2) 2) (~e "div" nil (into-array [(~interp bar)]))
         :else (~e "div" nil (into-array [4]))))))
