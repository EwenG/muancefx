(ns muancefx.utils)

(def Timer (js/Java.type "java.util.Timer"))

(def timer (Timer. "setTimeout" true))

(defn set-timeout [f millis]
  (.schedule timer f millis))

