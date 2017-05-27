(ns muancefx.core
  (:require [goog.object :as o])
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

(defn show-w []
  (.setTitle stage "Hellow World")
  (let [root (StackPane.)]
    (.setScene stage (Scene. root 300 250))
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
  (.setImplicitExit Platform false)
  
  (do (start-app)
      (run-later (show-w)))
  
  (.launch Application
           (.getClass my-application)
           (js/Java.to [] "java.lang.String[]"))

  (run-later (prn "e"))
  )
