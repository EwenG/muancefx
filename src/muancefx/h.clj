(ns muancefx.h
  (:require [muancefx.core :as m]))

(def javafx-nodes {'stack-pane 'javafx.scene.layout.StackPane
                   'anchor-pane 'javafx.scene.layout.AnchorPane
                   'v-box 'javafx.scene.layout.VBox
                   'h-box 'javafx.scene.layout.HBox
                   'checkbox 'javafx.scene.control.CheckBox
                   'button 'javafx.scene.control.Button
                   'text-field 'javafx.scene.control.TextField
                   'rectangle 'javafx.scene.shape.Rectangle
                   'text 'javafx.scene.text.Text
                   'label 'javafx.scene.control.Label
                   'list-view 'javafx.scene.control.ListView
                   'toggle-button 'javafx.scene.control.ToggleButton})

(def element-macros javafx-nodes)

(defmacro def-element-macros []
  `(do
     ~@(for [[name tag] element-macros]
         `(m/make-element-macro ~name ~tag))))

(def-element-macros)

(comment
  (macroexpand '(def-element-macros))
  )

(comment
  (macroexpand '(button :styleClass ["e"]
                        :style {:e "e"}))
  )
