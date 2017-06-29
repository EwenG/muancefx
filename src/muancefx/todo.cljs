(ns muancefx.todo
  (:require [muancefx.core :as m :include-macros true]
            [muancefx.utils :as utils]
            [muancefx.interop :as interop]
            [clojure.string])
  (:require-macros [muancefx.h :as h]))

(def Pos (js/Java.type "javafx.geometry.Pos"))
(def Double (js/Java.type "java.lang.Double"))
(def Priority (js/Java.type "javafx.scene.layout.Priority"))
(def VBox (js/Java.type "javafx.scene.layout.VBox"))
(def HBox (js/Java.type "javafx.scene.layout.HBox"))
(def AnchorPane (js/Java.type "javafx.scene.layout.AnchorPane"))
(def ToggleGroup (js/Java.type "javafx.scene.control.ToggleGroup"))
(def Insets (js/Java.type "javafx.geometry.Insets"))
(def KeyCode (js/Java.type "javafx.scene.input.KeyCode"))
(def Orientation (js/Java.type "javafx.geometry.Orientation"))
(def Font (js/Java.type "javafx.scene.text.Font"))
(def MouseEvent (.forName java.lang.Class "javafx.scene.input.MouseEvent"))

(.loadFont Font (.toExternalForm (utils/resource "fontawesome-webfont.ttf")) 10)

(declare todo-state)

(defn add-todo* [{:keys [counter items] :as todo-state} text]
  (-> todo-state
      (assoc-in [:items (inc counter)] {:id (inc counter) :title text :done false})
      (assoc :counter (inc counter))))

(defn add-todo [text]
  (swap! todo-state add-todo* text))

(defn toggle [id done] (swap! todo-state assoc-in [:items id :done] done))

(defn toggle-listener [o n state-ref id]
  (toggle id n))

(defn save [id title] (swap! todo-state assoc-in [:items id :title] title))

(defn delete [id] (swap! todo-state update :items dissoc id))

(defn delete-handler [e state-ref id]
  (delete id))

(defn complete-all* [items v]
  (->> (for [[id item] items]
         [id (assoc item :done v)])
       (into (empty items))))

(defn complete-all [v]
  (swap! todo-state update :items complete-all* v))

(defn complete-all-handler [o n state-ref]
  (complete-all n))

(defn clear-done* [items]
  (->> (for [[id item] items
             :when (not (:done item))]
         [id item])
       (into (empty items))))

(defn clear-done [e state-ref]
  (swap! todo-state update :items clear-done*))

(defn input-changed [o n state-ref]
  (reset! state-ref n))

(defn input-save [e state-ref]
  (let [v (-> @state-ref str clojure.string/trim)]
    (when-not (empty? v) (add-todo v))
    (reset! state-ref "")))

(defn input-keydown [e state-ref]
  (when (= KeyCode.ESCAPE (.getCode e))
    (reset! state-ref "")))

(m/defcomp todo-input []
  (h/text-field :id "addInput" :promptText "What needs to be done?"
                :text (m/state)
                ::m/hooks {:did-mount (fn []
                                        (.setHgrow HBox (m/javafx-node (m/vnode))
                                                   Priority.ALWAYS))}
                ::m/listen [[:text input-changed]]
                ::m/on [[:action input-save]
                        [:keyPressed input-keydown]]))

(defn set-editing [e state-ref]
  (when (> (.getClickCount e) 1)
    (swap! state-ref assoc :editing true)))

(defn unset-editing [e state-ref]
  (swap! state-ref assoc :editing false))

(defn focus-on-visible [o n state-ref]
  (when n
    (.requestFocus (m/javafx-node (m/vnode)))))

(defn edit-changed [o n state-ref]
  (swap! state-ref assoc :val n))

(defn edit-save [e state-ref id]
  (let [v (-> @state-ref :val str clojure.string/trim)]
    (when-not (empty? v) (save id v))
    (swap! state-ref assoc :editing false)))

(defn edit-focused-listener [o n state-ref id]
  (when-not n
    (let [v (-> @state-ref :val str clojure.string/trim)]
      (when-not (empty? v) (save id v))
      (swap! state-ref assoc :editing false))))

(m/defcomp todo-item [{:keys [id done title]}]
  (h/h-box
   :id "root"
   :alignment Pos.CENTER_LEFT
   :minHeight Double.NEGATIVE_INFINITY
   :styleClass "item_root"
   ::m/listen [:hover (fn [o n state-ref]
                        (swap! state-ref assoc :hover n))]
   ::m/hooks {:did-mount (fn []
                           (.setVgrow VBox (m/javafx-node (m/vnode)) Priority.ALWAYS))}
   (h/checkbox :id "completed" :mnemonicParsing false :selected done
               ::m/listen [:selected toggle-listener id])
   (h/stack-pane
    :alignment Pos.CENTER_LEFT
    ::m/hooks {:did-mount (fn [] (.setHgrow HBox (m/javafx-node (m/vnode)) Priority.ALWAYS))}
    (h/h-box
     :id "contentBox" :styleClass "content_box"
     :visible (not (:editing (m/state)))
     ::m/on [:mouseClicked set-editing]
     (h/label :id "contentLabel"
              :maxHeight 1.7976931348623157E308
              :maxWidth 1.7976931348623157E308
              :text title
              ::m/hooks {:did-mount (fn [] (.setHgrow HBox (m/javafx-node (m/vnode))
                                                      Priority.ALWAYS))})
     (h/button :id "deleteButton" :mnemonicParsing false :visible (boolean (:hover (m/state)))
               :styleClass "close_icon"
               :text "\uf00d"
               ::m/on [:action delete-handler id]))
    (h/text-field :id "contentInput" :promptText "What needs to be done?"
                  :visible (boolean (:editing (m/state)))
                  :text (str (:val (m/state)))
                  ::m/on [:action edit-save id]
                  ::m/listen [[:visible focus-on-visible]
                              [:focused edit-focused-listener id]
                              [:text edit-changed]]))))

(defn filter-handler [e state-ref name]
  (swap! todo-state assoc :filt name))

(defn control-button [toggle-group filt name text]
  (h/toggle-button
   :mnemonicParsing false
   :selected (= name filt)
   :text text
   :toggleGroup toggle-group
   ::m/on [:action filter-handler name]))

(m/defcomp controls [{:keys [active filt]}]
  (h/h-box
   :alignment Pos.CENTER
   :spacing "20.0"
   :padding (Insets. 5.0 5.0 5.0 5.0)
   (h/label
    :id "itemsLeftLabel" :text (str active " " (if (> active 1) "items" "item") " left"))
   (h/h-box
    :maxHeight Double.NEGATIVE_INFINITY
    :maxWidth Double.NEGATIVE_INFINITY
    :minHeight Double.NEGATIVE_INFINITY
    :minWidth Double.NEGATIVE_INFINITY
    :spacing "20.0"
    :padding (Insets. 5.0 5.0 5.0 5.0)
    (let [toggle-group (ToggleGroup.)]
      (control-button toggle-group filt :all "All")
      (control-button toggle-group filt :active "Active")
      (control-button toggle-group filt :done "Completed")))))

(defn display-item? [{:keys [done]} filt]
  (case filt
    :active (not done)
    :done done
    :all true))

(m/defcomp todo-app [{:keys [items filt]}]
  (let [items (vals items)
        done (->> items (filter :done) count)
        active (- (count items) done)]
    (h/v-box
     :alignment Pos.CENTER
     :maxHeight Double.NEGATIVE_INFINITY :maxWidth Double.NEGATIVE_INFINITY
     :minHeight Double.NEGATIVE_INFINITY :minWidth Double.NEGATIVE_INFINITY
     (h/label :id "title" :text "todos" :alignment Pos.CENTER_LEFT)
     (h/h-box
      :styleClass "add_item_root"
      :alignment Pos.CENTER
      (h/checkbox
       :id "selectAll"
       :mnemonicParsing false
       :visible (not (empty? items))
       :selected (zero? active)
       ::m/listen [:selected complete-all-handler])
      (todo-input))
     (h/anchor-pane
      :maxHeight Double.MAX_VALUE
      :maxWidth Double.MAX_VALUE
      :minHeight Double.NEGATIVE_INFINITY
      :minWidth Double.NEGATIVE_INFINITY
      :style {:-fx-background-color "grey"
              :-fx-border-color "-fx-box-border"
              :-fx-border-width "1 0 1 0"}
      ::m/hooks {:did-mount (fn []
                              (.setVgrow VBox (m/javafx-node (m/vnode)) Priority.ALWAYS))}
      (h/list-view
       :id "items"
       ::m/hooks {:did-mount (fn []
                               (.setTopAnchor AnchorPane (m/javafx-node (m/vnode)) 0)
                               (.setBottomAnchor AnchorPane (m/javafx-node (m/vnode)) 0)
                               (.setLeftAnchor AnchorPane (m/javafx-node (m/vnode)) 0)
                               (.setRightAnchor AnchorPane (m/javafx-node (m/vnode)) 0))}
       :items (for [todo items
                    :when (display-item? todo filt)]
                todo)
       :cellFactory todo-item))
     (controls {:active active :filt filt}))))

(defonce vtree (m/vtree))

(def init-state {:counter 0
                 :items (sorted-map)
                 :filt :all})

(defonce todo-state (atom init-state))

(add-watch todo-state ::todo-app (fn [k r o n] (m/patch vtree todo-app n)))

(comment

  (do (.setImplicitExit m/Platform false)
      (str (m/start-app)))

  (m/run-later
     (.setTitle m/stage "Todo app")
     (m/set-stage-vtree m/stage vtree)
     (.show m/stage))
  
  (do
    
    (m/patch vtree todo-app @todo-state)
    
    (.clear (.getStylesheets (.getScene m/stage)))
    (.add (.getStylesheets (.getScene m/stage))
          (.toExternalForm (utils/resource "muancefx/todo.css")))
    )

  (m/patch vtree todo-app @todo-state)
    
  (reset! todo-state init-state)

  (set! vtree (m/vtree))

  (js/goog.require "muancefx.todo" true)

  )

