(ns net.jeffhui.mith-test
  (:require [cljs.test :as t]
            ["mithril-query" :as mq]
            [net.jeffhui.mith :as m]))

(t/deftest example
  (t/is true)
  (t/is false)
  #_
  (let [out (mq (m/html [:div]))]
    (t/is (.first out "div"))))