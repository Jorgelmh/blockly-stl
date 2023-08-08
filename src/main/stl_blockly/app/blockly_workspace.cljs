(ns stl-blockly.app.blockly-workspace
  (:require ["blockly" :as Blockly]
            [clojure.string :as s]
            [org.parkerici.blockoid.core :as bo :refer [workspace]]
            [reagent.core :as r]
            ["@blockly/field-dependent-dropdown"]
            ["@blockly/block-dynamic-connection"]))

(set! *warn-on-infer* false)

(def inequility-operators {:lt  "<"
                           :gt  ">"
                           :geq ">="
                           :leq "<="})

(def connections {:nodes [{:id   "Spring-0wZwrU2wN"
                           :data {:label    "Spring1"
                                  :model-id "95BD75BD-26DF-47DD-B353-49A460FCF83F"}}
                          {:id   "Spring-0wZwrU2da"
                           :data {:label    "Spring2"
                                  :model-id "95BD75BD-26DF-47DD-B353-49A460FCF83F"}}
                          {:id   "Damper-sYICcw40H3"
                           :data {:label    "Damper1"
                                  :model-id "88EAF9B4-BAAB-449C-97A1-FEE85CDFE38C"}}
                          {:id   "Damper-sYICcw40H3"
                           :data {:label    "Mass1"
                                  :model-id "CBEBAEE0-2C4D-4746-B26C-BDAFC6C02247"}}]
                  :edges []})

(def model-vars [{:fmu/id     "88EAF9B4-BAAB-449C-97A1-FEE85CDFE38C"
                  :fmu/name   "Damper"
                  :fmu/outputs [{:variable/name "df_1"}
                                {:variable/name "df_2"}]}
                 {:fmu/id      "CBEBAEE0-2C4D-4746-B26C-BDAFC6C02247"
                  :fmu/name    "Mass"
                  :fmu/inputs  [{:variable/name "in_f_u"}
                                {:variable/name "in_f_w"}]
                  :fmu/outputs [{:variable/name "out_f_u"}
                                {:variable/name "out_f_w"}]}
                 {:fmu/id      "95BD75BD-26DF-47DD-B353-49A460FCF83F"
                  :fmu/name    "Spring"
                  :fmu/inputs  [{:variable/name "dis_xx"}
                                {:variable/name "dis_yx"}]
                  :fmu/outputs [{:variable/name "for_xx"}
                                {:variable/name "for_yx"}]}])

(def quantifiers {:always     "[]"
                  :eventually "<>"
                  :not         "!"})

(def quantifier-blocks (map (fn [block-name]
                              {:type      (name block-name)
                               :message0  (str (s/capitalize (name block-name)) " %1")
                               :args0     [{:type "input_value"
                                            :name  "condition"
                                            :check ["Boolean"]}]
                               :output    "Boolean"
                               :symbol    (block-name quantifiers)
                               :colour    230})
                            (keys quantifiers)))

(defn predicate-block [{:keys [nodes]} model-vars]
  (let [variables-map     (into {} (map (fn [{:keys [fmu/id fmu/outputs]}]
                                          [id (map :variable/name outputs)])
                                        model-vars))
        model-options     (map (fn [{:keys [data]}]
                                 [(:label data) (:label data)])
                               nodes)
        variable-options  (map (fn [{:keys [data]}]
                                 (let [{:keys [model-id label]} data
                                       outputs                  (get variables-map model-id)]
                                   [label (map #(vec [% %]) outputs)]))
                               nodes)
        dependent-options (into {} variable-options)]
    {:type     "predicate"
     :message0 "Model %1 Output port %2"
     :args0    [{:type    "field_dropdown"
                 :name    "MODEL_SELECTION"
                 :options model-options}
                {:type           "field_dependent_dropdown"
                 :name           "MODEL_VARIABLE"
                 :parentName     "MODEL_SELECTION"
                 :optionMapping  dependent-options
                 :defaultOptions [["None available", "noneAvailable"]]}]
     :output   "Predicate"}))

(def logical-operations [{:type        "logic_compare"
                          :message0    "%1 %2 %3"
                          :args0        [{:type  "input_value"
                                          :name  "VALUE1"
                                          :check ["Predicate" "Number"]}
                                         {:type    "field_dropdown"
                                          :name    "OPERATOR"
                                          :options (clj->js (mapv #(vec [(second %) (name (first %))]) inequility-operators))}
                                         {:type  "input_value"
                                          :name  "VALUE2"
                                          :check ["Predicate" "Number"]}]
                          :output       "Boolean"
                          :inputsInline true}
                         {:type         "logical_operation"
                          :message0     "%2 %1 %3"
                          :args0        [{:type  "input_value"
                                          :name  "A"
                                          :check ["Boolean"]}
                                         {:type    "field_dropdown"
                                          :name    "OP"
                                          :options [["and" "AND"]
                                                    ["or"  "OR"]]}
                                         {:type  "input_value"
                                          :name  "B"
                                          :check ["Boolean"]}]
                          :output       "Boolean"}
                         {:type         "implies"
                          :message0     "Implies %1 => %2"
                          :args0        [{:type  "input_value"
                                          :name  "VALUE1"
                                          :check ["Boolean"]}
                                         {:type  "input_value"
                                          :name  "VALUE2"
                                          :check ["Boolean"]}]
                          :inputsInline true
                          :output       "Boolean"}])

(def stl-blocks (concat quantifier-blocks
                        logical-operations
                        [(predicate-block connections model-vars)
                         {:type        "until"
                          :message0    "%1 U %2"
                          :args0       [{:type  "input_value"
                                         :name  "VALUE1"
                                         :check ["Boolean"]}
                                        {:type  "input_value"
                                         :name  "VALUE2"
                                         :check "Boolean"}]
                          :output       "Boolean"
                          :inputsInline true}]))

(defn add-custom-blocks [generator]
  (let [custom-blocks    (map (fn [{:keys [type symbol]}]
                                {:type type
                                 :fn   (case type 
                                         "predicate"
                                         (fn [block]
                                           (let [model  (.getFieldValue block "MODEL_SELECTION" 0)
                                                 port   (.getFieldValue block "MODEL_VARIABLE" 0)]
                                             (clj->js [(str model "." port) 0])))
                                         ("logic_compare" "implies" "until")
                                         (fn [block]
                                           (let [child-1            (.valueToCode generator block "VALUE1" 0)
                                                 child-2            (.valueToCode generator block "VALUE2" 0)
                                                 operator           (case (.-type block)
                                                                      "logic_compare"
                                                                      (->> (.getFieldValue block "OPERATOR" 0)
                                                                           keyword
                                                                           (get inequility-operators))
                                                                      "implies"
                                                                      "->"
                                                                      "until"
                                                                      "U"
                                                                      "->")]
                                             (clj->js [(str child-1 " " operator " " child-2) 0])))
                                         "logical_operation"
                                         (fn [block]
                                           (let [operator         (if (= (.getFieldValue block "OP") "AND") "&" "|")
                                                 order            (if (= operator "&") 13 14)
                                                 default-argument (if (= operator "&") "true" "false")
                                                 block-1          (.valueToCode generator block "A" order)
                                                 block-2          (.valueToCode generator block "B" order)
                                                 arg-0            (if (= block-1 "") default-argument block-1)
                                                 arg-1            (if (= block-2 "") default-argument block-2)]
                                             (clj->js [(str arg-0 " " operator " " arg-1) 0])))
                                         (fn [block]
                                           (let [members (.valueToCode generator block "condition" 0)]
                                             (clj->js [(str symbol " (" members ")") 0]))))})
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
    (aset (.-forBlock generator) "math_number" (fn [block]
                                                 (let [value (.getFieldValue block "NUM" 0)
                                                       value (if (= value "") 0 value)]
                                                   (clj->js [(str value) 0]))))
    (aset (.-forBlock generator) "logic_boolean" (fn [block]
                                                   (clj->js [(s/lower-case (.getFieldValue block "BOOL" 0)) 0])))
    (add-custom-blocks generator)))

(def stl-generator (create-blockly-generator))

(def toolbox
  `[:toolbox
    [:category "STL" {}
     [:block "math_number"]
     [:block "logic_boolean"]
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
