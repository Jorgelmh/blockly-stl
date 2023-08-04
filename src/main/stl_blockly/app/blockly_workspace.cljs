(ns stl-blockly.app.blockly-workspace
  (:require [org.parkerici.blockoid.core :as bo :refer [workspace]]
            ["blockly" :as Blockly]
            [goog.object :as g]
            [reagent.core :as r]))

(defn create-blockly-generator []
  (let [add-custom-block  (fn [generator name fn]
                            (aset (.-forBlock generator) name fn))
        generator         (Blockly/Generator. "STL")]
    (aset generator "PRECEDENCE" 0)
    (-> generator
        (add-custom-block "always" (fn [block]
                                     (println block)
                                     "always")))
    generator))

(def stl-generator (create-blockly-generator))

(def stl-blocks [{:type     "always"
                  :message0 "%1"
                  :args0    [{:type "input_statement"
                              :name "members"}]
                  :colour   230}])

(def toolbox
  `[:toolbox
    [:category "Arithmetic" {}
     ~@(cons [:block "math_number" {} [:field "NUM" 123]]
             (mapv (fn [block] [:block (:type block)]) stl-blocks))]])

(defn run-code []
  (println stl-generator)
  (print (.workspaceToCode ^js/Object stl-generator @workspace)))

;;; A near-copy of the toolbox in Blockly demo: https://blockly-demo.appspot.com/static/demos/toolbox/index.html

(defn initialize []
  (println "Hello")
  (bo/define-blocks stl-blocks)
  (bo/define-workspace
    "blockly-div"
    (bo/toolbox toolbox)
    {}
    (fn []
      (run-code))))

(defn blockoid-container []
  (r/create-class
   {:component-did-mount (fn []
                           (initialize))
    :reagent-render (fn []
                      [:div {:id "blockly-div"}])}))
