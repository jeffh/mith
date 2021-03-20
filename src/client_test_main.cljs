(ns client-test-main
  (:require [cljs.test :as t]))

(defn start []
  (js/Object.assign js/global.window
                    ((js/require "mithril/test-utils/domMock.js"))
                    ((js/require "mithril/test-utils/pushStateMock")))
  (set! js/global.requestAnimationFrame (fn [cb] (js/setTimeout cb (/ 1000 60))))
  (t/run-tests))

(defn stop [done]
  (done))

(defn ^:export init []
  (start))