(ns stl-blockly.app.core
  (:require [reagent.dom :as rdom]
            [cljsjs.blockly]
            [stl-blockly.app.blockly-workspace :refer [blockoid-container]]))

(defn app []
  [blockoid-container])

(defn render []
  (rdom/render [app] (.getElementById js/document "root")))

(defn ^:export main []
  (render))

(defn ^:dev/after-load reload! []
  (render))
