(ns stl-blockly.app.blockly-workspace
  (:require ["blockly" :as Blockly]
            [clojure.string :as s]
            [org.parkerici.blockoid.core :as bo :refer [workspace]]
            [reagent.core :as r]))

(set! *warn-on-infer* false)


(def quantifiers {:always     "[]"
                  :eventually "<>"
                  :not         "!"})

(def quantifier-blocks (map (fn [block-name]
                              {:type      (name block-name)
                               :message0  (str (s/capitalize (name block-name)) " %1")
                               :args0     [{:type "input_value"
                                            :name "condition"}]
                               :output    "Boolean"
                               :symbol    (block-name quantifiers)
                               :colour    230})
                            (keys quantifiers)))

(def stl-blocks quantifier-blocks)

(defn add-custom-blocks [generator]
  (let [custom-blocks    (map (fn [{:keys [type symbol]}]
                                {:type type
                                 :fn   (fn [block]
                                         (let [members (-> (.valueToCode generator block "condition" 13))]
                                           (println members)
                                           (clj->js [(str symbol " (" members ")") 0])))})
                              stl-blocks)]
    (loop [remaining-blocks  custom-blocks
           updated-generator generator]
      (if (seq remaining-blocks)
        (let [{:keys [type fn]} (first remaining-blocks)]
          (aset (.-forBlock generator) type fn)
          (recur (rest remaining-blocks)
                 updated-generator))
        updated-generator))))

(defn create-blockly-generator []
  (let [generator        (Blockly/Generator. "STL")]
    (aset generator "PRECEDENCE" 0)
    (aset generator "ORDER_OVERRIDES" (clj->js [[13 13]
                                               [14 14]]))
    (aset (.-forBlock generator) "logic_compare" (fn [block]
                                                   (let [operator (.getFieldValue block "OP")]
                                                     (println operator))))
    (aset (.-forBlock generator) "logic_operation" (fn [block]
                                                     (let [operator         (if (= (.getFieldValue block "OP") "AND") "&" "|")
                                                           order            (if (= operator "&") 13 14)
                                                           default-argument (if (= operator "&") "true" "false")
                                                           block-1          (.valueToCode generator block "A" order)
                                                           block-2          (.valueToCode generator block "B" order)
                                                           arg-0            (if (= block-1 "") default-argument block-1)
                                                           arg-1            (if (= block-2 "") default-argument block-2)]
                                                       (clj->js [(str arg-0 " " operator " " arg-1) order]))))
    (add-custom-blocks generator)))

(def stl-generator (create-blockly-generator))

(def toolbox
  `[:toolbox
    [:category "Logic" {:colour "%{BKY_LOGIC_HUE}"}
     [:block "logic_compare"]
     [:block "logic_operation"]]
    [:category "STL" {}
     ~@(mapv (fn [block] [:block (:type block)]) stl-blocks)]])

(defn run-code []
  (let [stl-code (.workspaceToCode ^js/Object stl-generator @workspace)]
    (-> js/document
        (.getElementById "output-text")
        (.-innerHTML)
        (set! stl-code))))

;;; A near-copy of the toolbox in Blockly demo: https://blockly-demo.appspot.com/static/demos/toolbox/index.html

(defn initialize []
  (bo/define-blocks stl-blocks)
  (bo/define-workspace
    "blockly-canvas"
    (bo/toolbox toolbox)
    {}
    (fn []
      (run-code))))

(defn blockoid-container []
  (r/create-class
   {:component-did-mount (fn []
                           (initialize))
    :reagent-render (fn []
                      [:<>
                       [:div {:id "blockly-canvas"}]
                       [:div {:id "blockly-output"}
                        [:code#output-text]]])}))
