(ns cljsjs.blockly
  (:require ["blockly" :as blockly]))

(js/goog.exportSymbol "Blockly" blockly)
