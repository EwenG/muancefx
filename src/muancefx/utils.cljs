(ns muancefx.utils)

(def Timer (js/Java.type "java.util.Timer"))

(defn set-timeout [f millis]
  (let [timer (Timer. "setTimeout" true)]
    (.schedule timer f millis)
    timer))

(defn clear-timeout [timer]
  (.cancel timer))

(defn set-interval [f millis]
  (let [timer (Timer. "setInterval" true)]
    (.schedule timer f millis millis)
    timer))

(defn clear-interval [timer]
  (.cancel timer))

