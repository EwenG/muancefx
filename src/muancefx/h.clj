(ns muancefx.h
  (:require [muancefx.core :as m]))

(def javafx-nodes {'stack-pane 'javafx.scene.layout.StackPane
                   'button 'javafx.scene.control.Button})

(def element-macros javafx-nodes)

(defmacro def-element-macros []
  `(do
     ~@(for [[name tag] element-macros]
         `(m/make-element-macro ~name ~tag))))

(def-element-macros)

(comment
  (macroexpand '(def-element-macros))
  )
