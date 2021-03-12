(ns net.jeffhui.mith.demo.example
  (:require [net.jeffhui.mith :as mith :refer [html]]
            [net.jeffhui.mith.ratom :as ratom]))

(def t (ratom/ratom (js/Date.)))
(defonce iv (js/setInterval #(reset! t (js/Date.)) 1000))

(def tt (ratom/reaction #(str (js/Date.))))

(def header
  (mith/component
   #(html [:h1 (str @t) "YOOO" @tt])))

(defn counter [vnode]
  (let [cnt (ratom/ratom 0)]
    (mith/component
     {:view (fn [_]
              (html [:div
                     [:p "Count: " @cnt]
                     [:button {:onclick (fn [_] (swap! cnt inc))} "Increment Count"]]))
      :onupdate (fn [vnode] (js/console.log "UPDATED DOM"))})))

(defn root-view []
  (html [:div.foo#baz [:h1 "hello"] [header]
         [counter]]))

(defn main []
  (mith/mount (js/document.getElementById "root") #js{:view root-view}))

(defn ^:dev/after-load reloaded []
  (main))