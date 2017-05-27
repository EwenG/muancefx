(ns muancefx.core)

(defmacro run-later [& body]
  `(let [runnable# (js/Java.extend muancefx.core/Runnable
                                   (cljs.core/js-obj "run" (cljs.core/fn [] ~@body)))]
     (.runLater muancefx.core/Platform (new runnable#))))
