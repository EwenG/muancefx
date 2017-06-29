(ns muancefx.h
  (:require [muancefx.core :as m]))

(def javafx-nodes #{{:name 'text :tag 'javafx.scene.text.Text :max-children 0}
                    {:name 'stack-pane :tag 'javafx.scene.layout.StackPane}
                    {:name 'anchor-pane :tag 'javafx.scene.layout.AnchorPane}
                    {:name 'scroll-pane :tag 'javafx.scene.control.ScrollPane
                     :children-getter "contentProperty" :max-children 1}
                    {:name 'v-box :tag 'javafx.scene.layout.VBox}
                    {:name 'h-box :tag 'javafx.scene.layout.HBox}
                    {:name 'checkbox :tag 'javafx.scene.control.CheckBox}
                    {:name 'button :tag 'javafx.scene.control.Button}
                    {:name 'text-field :tag 'javafx.scene.control.TextField}
                    {:name 'rectangle :tag 'javafx.scene.shape.Rectangle}
                    {:name 'label :tag 'javafx.scene.control.Label}
                    {:name 'toggle-button :tag 'javafx.scene.control.ToggleButton}
                    {:name 'scroll-bar :tag 'javafx.scene.control.ScrollBar}
                    {:name 'toolbar :tag 'javafx.scene.control.ToolBar}
                    {:name 'list-view :tag 'javafx.scene.control.ListView :max-children 0}})

(def element-macros javafx-nodes)

(defmacro def-element-macros []
  `(do
     ~@(for [{:keys [name tag children-getter max-children]} element-macros]
         `(m/make-element-macro
           ~name ~tag ~(or children-getter "getChildren") ~max-children))))

(def-element-macros)

(comment
  (macroexpand '(def-element-macros))
  )

(comment
  (macroexpand '(button :styleClass ["e"]
                        :style {:e "e"}))
  )
