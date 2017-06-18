(ns muancefx.utils)

(def Timer (js/Java.type "java.util.Timer"))
(def Thread (js/Java.type "java.lang.Thread"))

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

(defn resource
  ([n]
   (resource n (.getContextClassLoader (.currentThread Thread))))
  ([n loader]
   (.getResource loader n)))
