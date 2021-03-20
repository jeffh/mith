(ns net.jeffhui.mith.demo.example
  (:require [net.jeffhui.mith :as mith :refer [html]]
            [net.jeffhui.mith.ratom :as ratom]))

(def t (ratom/ratom (str (js/Date.))))
;; swapping the atom should trigger mithril draws
(defonce iv (js/setInterval #(reset! t (str (js/Date.))) 500))

;; this should only react if another ratom needs to be updated
(def tt (ratom/reaction #(str (js/Date.))))

(def header
  (mith/component
   {:view #(html [:h1 @t "YO" @tt])
    #_#_:onupdate (fn [_] (js/console.log "header update"))}))

(defn counter [vnode]
  ;; A method to create local state without triggering lots of unnecessary re-renders
  (let [cnt (ratom/ratom 0)]
    (mith/component
     {:view (fn [_]
              (html [:div
                     [:p "Count: " @cnt]
                     [:button {:onclick (fn [_] (swap! cnt inc))} "Increment Count"]]))
      #_#_:onupdate (fn [vnode] (js/console.log "UPDATED DOM"))})))

(defn root-view []
  (html [:div.foo#baz [:h1 "hello"] [header]
         [counter]]))

(defn main []
  (mith/mount (js/document.getElementById "root") #js{:view root-view}))

(defn ^:dev/after-load reloaded []
  (main))