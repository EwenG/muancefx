(ns muancefx.core
  (:refer-clojure :exclude [remove key])
  (:require [muance.core :as m]
            [goog.object :as o]
            [muancefx.interop :as interop]
            [muancefx.utils :as utils])
  (:require-macros [muancefx.core :refer [run-later]]))

(def Platform (js/Java.type "javafx.application.Platform"))
(def Runnable (js/Java.type "java.lang.Runnable"))
(def CompletableFuture (js/Java.type "java.util.concurrent.CompletableFuture"))

(defonce stage nil)
(def started-promise (CompletableFuture.))

#_(def Button (js/Java.type "javafx.scene.control.Button"))
(def Stage (js/Java.type "javafx.stage.Stage"))
(def EventHandler (js/Java.type "javafx.event.EventHandler"))
(def StackPane (js/Java.type "javafx.scene.layout.StackPane"))
(def Scene (js/Java.type "javafx.scene.Scene"))
(def Group (js/Java.type "javafx.scene.Group"))
(def String (js/Java.type "java.lang.String"))

(defn show-w []
  (.setTitle stage "Hello World")
  (let [root (StackPane.)
        Button (js/Java.type "javafx.scene.control.Button")
        button (Button.)
        button2 (Button.)]
    (.setScene stage (Scene. root 300 250))
    (.add (.getChildren root) 0 button)
    (.add (.getChildren root) 0 button2)
    (prn (str (.getChildren root)))
    (.show stage)))

(defn start [s]
  (set! stage s)
  (.complete started-promise s))

(defn ^:export start-app []
  (let [Application (js/Java.type "javafx.application.Application")
        MuanceFxApp (js/Java.extend Application #js {:start start})
        AppThread (js/Java.extend
                   (js/Java.type "java.lang.Thread")
                   #js {:run (fn [] (.launch Application
                                             (.getClass (MuanceFxApp.))
                                             (js/Java.to [] "java.lang.String[]")))})]
    (.start (AppThread.))
    (.get started-promise)))

(defn ^:export start-my-app []
  (start-app)
  (run-later (show-w)))

(comment
  (do (.setImplicitExit Platform false)
      (str (start-app)))

  (run-later (show-w))
  
  (.launch Application
           (.getClass my-application)
           (js/Java.to [] "java.lang.String[]"))

  (run-later (prn "e"))
  )

(defn state []
  (assert (not (nil? m/*component*)) (str "muancefx.core/state was called outside a render loop"))
  m/*state*)

(defn vnode []
  (assert (not (nil? m/*component*)) (str "muancefx.core/vnode was called outside a render loop"))
  m/*vnode*)

(defn component-name
  "Return the fully qualified name of the node's component, as a string."
  [vnode]
  (assert vnode "muancefx.core/component-name expects a vnode.")
  (m/component-name vnode))

(defn javafx-nodes
  "Return an javascript array of all the javafx nodes associated with vnode."
  [vnode]
  (assert vnode "muancefx.core/dom-nodes expects a vnode.")
  (if (m/component? vnode)
    (m/dom-nodes* #js [] vnode)
    #js [(aget vnode m/index-node)]))

(defn javafx-node
  "Return the javafx nodes associated with vnode. Returns the first children of vnode if vnode is
  a component and is associated with multiple javafx nodes."
  [vnode]
  (assert vnode "muancefx.core/dom-node expects a vnode.")
  (if (m/component? vnode)
    (m/ref-node-down vnode)
    (aget vnode m/index-node)))

(defn key
  "Returns the :muancefx.core/key attribute of vnode, as a string."
  [vnode]
  (assert vnode "muancefx.core/key expects a vnode.")
  (aget vnode m/index-key))

(defn set-timeout
  "Execute f after a delay expressed in milliseconds. The first argument of f is the local state reference of the vnode component."
  [vnode f millis]
  (assert vnode "muancefx.core/set-timeout expects a vnode.")
  (let [component (if (m/component? vnode) vnode (aget vnode m/index-component))
        state-ref (aget component m/index-comp-data m/index-comp-data-state-ref)]
    (utils/set-timeout (fn [] (f state-ref)) millis)))

(defn set-interval
  "Periodically execute f. The period is expressed in milliseconds. The first argument of f is the local state reference of the vnode component."
  [vnode f millis]
  (assert vnode "muancefx.core/set-timeout expects a vnode.")
  (let [component (if (m/component? vnode) vnode (aget vnode m/index-component))
        state-ref (aget component m/index-comp-data m/index-comp-data-state-ref)]
    (utils/set-interval (fn [] (f state-ref)) millis)))

(defn remove-node
  "Remove a javafx node. Do nothing if the node has no parent."
  [node]
  (when-let [p (.getParent node)]
    (let [children (.getChildren p)
          index (.indexOf children node)]
      (when-not (== index -1)
        (.remove children index)))))

(defn- insert-fn-javafx [parent-node vnode ref-node]
  (let [children (.getChildren parent-node)
        index (.indexOf children ref-node)]
    (if (== index -1)
      (.add children (aget vnode m/index-node))
      (.add children (.indexOf children ref-node) (aget vnode m/index-node)))))

(defn- set-property [node ns key val]
  (interop/invokePropertySetter node key val))

(defn- set-input-value [node ns key val]
  (when (not= (interop/invokePropertyGetter node "getText") val)
    (interop/invokePropertySetter node "setText" val)))

(defn- set-class [node ns key val]
  (let [style-class (.getStyleClass node)]
    (.clear style-class)
    (.add style-class val)))

(defn- set-classes [node ns key val]
  (let [style-class (.getStyleClass node)]
    (.clear style-class)
    (let [l (.-length val)]
      (loop [i 0]
        (when (< i l)
          (.add style-class (aget val i))
          (recur (inc i)))))))

(defn- create-element [tag]
  ;; tag is nil when opening a component
  (when tag
    (let [c (js/Java.type tag)]
      (c.))))

(defn- handle-event-handler [node key prev-handler handler]
  (interop/invokePropertySetter node key handler))

(defn- make-handler-fn-0 [f state-ref]
  (let [handler-class (js/Java.extend
                       EventHandler
                       #js {:handle
                            (fn [e] (f e state-ref))})]
    (handler-class.)))

(defn- make-handler-fn-1 [f state-ref param1]
  (let [handler-class (js/Java.extend
                       EventHandler
                       #js {:handle
                            (fn [e] (f e state-ref param1))})]
    (handler-class.)))

(defn- make-handler-fn-2 [f state-ref param1 param2]
  (let [handler-class (js/Java.extend
                       EventHandler
                       #js {:handle
                            (fn [e] (f e state-ref param1 param2))})]
    (handler-class.)))

(defn- make-handler-fn-3 [f state-ref param1 param2 param3]
  (let [handler-class (js/Java.extend
                       EventHandler
                       #js {:handle
                            (fn [e] (f e state-ref param1 param2 param3))})]
    (handler-class.)))

(def context-javafx #js {:insert-fn insert-fn-javafx
                         :remove-node-fn remove-node
                         :create-element-fn create-element
                         :handle-event-handler-fn handle-event-handler
                         :make-handler-0 make-handler-fn-0
                         :make-handler-1 make-handler-fn-1
                         :make-handler-2 make-handler-fn-2
                         :make-handler-3 make-handler-fn-3})

(defn- on [key f]
  (m/on-impl context-javafx key f nil nil nil 0))

(defn- on-static [key f]
  (when (and (> m/*new-node* 0) (fn? f))
    (let [node (aget m/*vnode* m/index-node)
          state-ref (aget m/*component* m/index-comp-data m/index-comp-data-state-ref)]
      (handle-event-handler node key nil (make-handler-fn-0 f state-ref)))))

(defn- on1 [key f attr1]
  (m/on-impl context-javafx key f attr1 nil nil 1))

(defn- on-static1 [key f attr1]
  (when (and (> m/*new-node* 0) (fn? f))
    (let [node (aget m/*vnode* m/index-node)
          state-ref (aget m/*component* m/index-comp-data m/index-comp-data-state-ref)]
      (handle-event-handler node key nil (make-handler-fn-1 f state-ref attr1)))))

(defn- on2 [key f attr1 attr2]
  (m/on-impl context-javafx key f attr1 attr2 nil 2))

(defn- on-static2 [key f attr1 attr2]
  (when (and (> m/*new-node* 0) (fn? f))
    (let [node (aget m/*vnode* m/index-node)
          state-ref (aget m/*component* m/index-comp-data m/index-comp-data-state-ref)]
      (handle-event-handler node key nil (make-handler-fn-2 f state-ref attr1 attr2)))))

(defn- on3 [key f attr1 attr2 attr3]
  (m/on-impl context-javafx key f attr1 attr2 attr3 3))

(defn- on-static3 [key f attr1 attr2 attr3]
  (when (and (> m/*new-node* 0) (fn? f))
    (let [node (aget m/*vnode* m/index-node)
          state-ref (aget m/*component* m/index-comp-data m/index-comp-data-state-ref)]
      (handle-event-handler node key nil (make-handler-fn-3 f state-ref attr1 attr2 attr3)))))

(defn- prop [key val]
  (m/attr-impl m/*vnode* nil key val set-property))

(defn- prop-static [key val]
  (when (and (> m/*new-node* 0) (not (nil? val)))
    (let [node (aget m/*vnode* m/index-node)]
      (set-property node nil key val))))

(defn- style-class [c]
  (m/attr-impl m/*vnode* nil "styleClass" c set-class))

(defn- style-class-static [c]
  (when (and (> m/*new-node* 0) (not (nil? c)))
    (let [node (aget m/*vnode* m/index-node)]
      (set-class node nil "styleClass" c))))

(defn- style-classes [classes]
  (m/attr-impl m/*vnode* nil "styleClass" classes set-classes))

(defn- style-classes-static [classes]
  (when (and (> m/*new-node* 0) (not (nil? classes)))
    (let [node (aget m/*vnode* m/index-node)]
      (set-classes node nil "styleClass" classes))))

(defn- input-value [val]
  (m/attr-impl m/*vnode* nil nil (when (not (nil? val)) (str val)) set-input-value))

(defn- style-remove-nils! [style-arr i l]
  (when (< i l)
    (let [v (aget style-arr (inc i))]
      (when (= (.trim v) "")
        (aset style-arr i "")
        (aset style-arr (inc i) "")
        (aset style-arr (+ i 2) "")))
    (recur style-arr (+ i 3) l)))

(defn- style [key val]
  (style-remove-nils! val 0 (.-length val))
  (m/attr-impl m/*vnode* nil key (.join String "" val) set-property))

(defn- style-static [key val]
  (style-remove-nils! val 0 (.-length val))
  (when (and (> m/*new-node* 0) (not= 0 (.-length val)))
    (let [node (aget m/*vnode* m/index-node)]
      (set-property node nil key (.join String "" val)))))

(defn- open [tag typeid key will-update will-unmount remove-hook]
  (m/open-contextualized context-javafx tag typeid key will-update will-unmount remove-hook))

(defn- close [did-mount did-update]
  (m/close-contextualized context-javafx did-mount did-update))

(defn- open-comp [component-name typeid props? props comp-fn key hooks]
  (m/open-comp-contextualized
   context-javafx component-name typeid props? props comp-fn key hooks))

(defn- close-comp [parent-component hooks]
  (m/close-comp-contextualized context-javafx parent-component hooks))

(defn- patch-root-impl [vtree patch-fn props]
  ;; Otherwise, set the component at the top as dirty and update its props and patch-fn
  (let [vnode (.-vnode vtree)
        render-queue (.-render-queue vtree)
        async (aget render-queue m/index-render-queue-async)
        children (aget vnode m/index-children)]
        ;; comp is nil on first render
    (if-let [comp (aget children 0)]
      (do
        (if-let [dirty-comps (aget render-queue m/index-render-queue-offset)]
          (do
            (aset dirty-comps 0 props)
            (aset dirty-comps 1 patch-fn)
            (aset dirty-comps 2 comp))
          (aset render-queue m/index-render-queue-offset #js [props patch-fn comp]))
        (aset (aget comp m/index-comp-data) m/index-comp-data-dirty-flag true)
        (when-not (aget render-queue m/index-render-queue-dirty-flag)
          (aset render-queue m/index-render-queue-dirty-flag true)
          (if async
            (run-later (m/process-render-queue render-queue))
            (m/process-render-queue render-queue))))
      (run-later
       (m/patch-impl render-queue vnode nil patch-fn props false)
       (m/process-post-render-hooks render-queue)))))

;; id is used to keep track of rendered vtrees, for re-rendering on function/component reload
(deftype VTree [vnode render-queue id])

(defn post-render
  "Registers a function to be executed after the next Muancefx render pass. 
  Takes a vnode or vtree, the function to be executed and up to three optional parameters 
  to be passed to the function f."
  ([vnode f]
   (assert vnode "muancefx.core/post-render expects a vnode.")
   (m/post-render vnode f))
  ([vnode f arg1]
   (assert vnode "muancefx.core/post-render expects a vnode.")
   (m/post-render vnode f arg1))
  ([vnode f arg1 arg2]
   (assert vnode "muancefx.core/post-render expects a vnode.")
   (m/post-render vnode f arg1 arg2))
  ([vnode f arg1 arg2 arg3]
   (assert vnode "muancefx.core/post-render expects a vnode.")
   (m/post-render vnode f arg1 arg2 arg3)))

(defonce ^{:private true} vtree-ids (volatile! 0))
(defonce ^{:private true} roots #js {})

(defn- new-root-vnode []
  #js [nil nil (Group.) nil 0 #js []])

(defn- refresh-roots []
  (o/forEach roots m/refresh-root))

(defn vtree
  "Creates a new vtree."
  []
  (VTree. (new-root-vnode)
          ;; post render hooks
          #js [true #js []]
          (vswap! vtree-ids inc)))

(defn patch
  "Patch a vtree using component. The optional third argument is the component props."
  ([vtree component]
   (patch-root-impl vtree component m/no-props-flag))
  ([vtree component props]
   (patch-root-impl vtree component props)))

(defn scene [vtree & {:keys [width height depth-buffer anti-aliasing fill]}]
  (let [vnode (.-vnode vtree)
        root (aget vnode m/index-node)
        s (Scene. root width height depth-buffer anti-aliasing)]
    (when (some? fill)
      (.setFill s fill))
    s))

