(ns muancefx.core
  (:require [clojure.string :as string]
            [cljs.analyzer :as ana]
            [cljs.repl])
  (:import [cljs.tagged_literals JSValue]
           [muancefx JUtils]
           [clojure.lang RT]
           [java.beans PropertyDescriptor]))

(defonce typeid (atom 1))
(defonce comp-typeid (atom -1))

(defn inc-typeid [t]
  ;; MAX_SAFE_INTEGER
  (if (= t 9007199254740991) 1 (inc t)))

(defn dec-comp-typeid [t]
  ;; MIN_SAFE_INTEGER
  (if (= t -9007199254740991) -1 (dec t)))

(defn- safe-symbol [x]
  (when x (symbol x)))

(defn- resolve-namespace
  [env sym ns]
  (if (= 'cljs.core sym)
    sym
    (-> (merge (:requires ns) (:require-macros ns))
        (get sym))))

(defn- cljs-resolve [env sym]
  (when (and sym (not (get-in env [:locals sym])))
    (let [sym-ns (safe-symbol (namespace sym))
          ns-name (if sym-ns
                    (resolve-namespace env sym-ns (:ns env))
                    (get-in env [:ns :name]))]
      (symbol (str ns-name) (name sym)))))

(declare compile-element-macro)
(declare text)

(defn- compile-form [env form]
  (cond (and (seq? form) (symbol? (first form)))
        (let [var (cljs-resolve env (first form))
              clj-var (resolve var)]
          (cond (::tag (meta clj-var))
                (compile-element-macro env (::tag (meta clj-var)) nil (rest form))
                :else form))
        (string? form) `(muancefx.core/text ~form)
        :else form))

(defn- local-dep [{name :name fn-var :fn-var
                   local :local init :init
                   op :op tag :tag dynamic :dynamic
                   ns :ns :as info}]
  (cond
    ;; fn param
    (and (= :var op) (not local) (not fn-var))
    info
    ;; dynamic var
    dynamic
    info
    ;; The analyzed var is a local var with an initial binding. The
    ;; initial binding may itself depend on a function parameter,
    ;; thus we recurse on the initial binding
    (and (= :var op) local init)
    (if (not (empty? (:children init)))
      ;; If init has children, then it is a complex expression, by
      ;; opposition to a single var or local binding. Recurse on every
      ;; child in order to analyze each expression individually
      (->> (:children init) (mapv :info) (some local-dep))
      (recur (:info init)))
    :else nil))

(defn- static-symbol? [env s]
  (if-let [local (get (:locals env) s)]
    (not (local-dep local))
    (not (= 'muance.core/*state* (cljs-resolve env s)))))

(defn- static? [env x]
  (cond (nil? x) true
        (boolean? x) true
        (string? x) true
        (keyword? x) true
        (number? x) true
        (and (seq? x) (= (first x) `quote)) true
        (vector? x) (every? (partial static? env) x)
        (map? x) (every? (partial static? env) x)
        (symbol? x) (static-symbol? env x)
        :else false))

;; Like clojure.string/capitalize but do not lowercase other letters
(defn ^String capitalize [^CharSequence s]
  (let [s (.toString s)]
    (if (< (count s) 2)
      (.toUpperCase s)
      (str (.toUpperCase (subs s 0 1)) (subs s 1)))))

(defn- as-str [x]
  (cond (string? x) x
        (keyword? x) (name x)
        :else `(cljs.core/str ~x)))

(defn as-property [class-name property]
  (let [property-method (str (name property) "Property")
        property-method-name (->> (.getMethods
                                   (Class/forName class-name false (RT/baseLoader)))
                                  (into [])
                                  (filter #(.startsWith (.getName %) property-method))
                                  first)]
    (when property-method-name (.getName property-method-name))))

(defn- property-with-on [property]
  (->> (name property) capitalize (str "on")))

(defn- as-on-property [class-name property]
  (as-property class-name (property-with-on property)))

(comment
  (as-property "javafx.scene.control.TextField" "onAction")
  (as-on-property "javafx.scene.control.TextField" :action)

  (as-property "javafx.scene.control.Button" :onMouseClicked)
  (as-on-property "javafx.scene.control.Button" :mouseClicked)
  )

(def ^{:private true} props-to-rename
  {:class :className
   :for :htmlFor
   :accept-charset :acceptCharset
   :http-equiv :httpEquiv})

(defn- rename-prop [[k v :as prop]]
  (if-let [new-k (get props-to-rename k)]
    [new-k v]
    prop))

(defn- attributes [body]
  (let [attrs (->> (partition 2 body)
                   (take-while (comp keyword? first))
                   (map rename-prop))]
    (when (not (empty? attrs))
      (let [attrs-keys (map first attrs)]
        (assert (apply distinct? attrs-keys)
                (str "duplicate attributes: " (pr-str attrs-keys)))
        (let [attrs-keys (remove #(or (= ::hooks %) (= ::key %) (= ::on %)) attrs-keys)]
          (when (not (empty? attrs-keys))
            (assert (apply distinct? (map name attrs-keys))
                    (str "duplicate attributes: " (pr-str attrs-keys)))))))
    (into {} (map vec attrs))))

(defn- handler? [h]
  (and (vector? h) (keyword? (first h))))

(defn- validate-attributes [{:keys [::hooks style ::on] :as attributes}]
  (when (contains? attributes ::hooks) (assert (map? hooks)))
  (when (contains? attributes :style) (assert (map? style)))
  (when (contains? attributes ::on) (assert (or (handler? on) (every? handler? on)))))

(defn- body-without-attributes [body attributes]
  (drop (* 2 (count attributes)) body))

(defn- class-call [env class]
  (if (vector? class)
    (if (every? (partial static? env) class)
      (if (some symbol? class)
        (let [classes-with-spaces (-> class (interleave (repeat " ")) butlast)]
          `(prop-static "className" (cljs.core/str ~@classes-with-spaces)))
        `(prop-static "className" ~(reduce #(if (nil? %1) (str %2) (str %1 " " %2)) nil class)))
      (let [classes-with-spaces (-> class (interleave (repeat " ")) butlast)]
        `(prop "className" (cljs.core/str ~@classes-with-spaces))))
    (if (static? env class)
      `(prop-static "className" ~class)
      `(prop "className" ~class))))

(defn- style-calls [env style]
  (map (fn [[k v]]
         (if (string/starts-with? (str k) ":--")
           (if (static? env v)
             `(style-custom-static ~(as-str k) ~v)
             `(style-custom ~(as-str k) ~v))
           (if (static? env v)
             `(style-static ~(as-str k) ~v)
             `(style ~(as-str k) ~v))))
       style))

(defn- format-style-entry [[k v]]
  (if (string? v)
    [(str (name k) ":") (str v) ";"]
    [(str (name k) ":") `(cljs.core/str ~v) ";"]))

(defn- style-map->arr [style-map]
  (-> (mapcat format-style-entry style-map)
      (conj `cljs.core/array)))

(comment
  (style-map->arr {:display "block"
                   :height 'x
                   :width "33px"})
  (style-map->arr {})

  (style-map->arr {:display nil})
  )

;; Wrap event handlers in a var when in dev compilation mode in order to enable the possibility
;; to dynamically redefine event handler functions definition
(defn- maybe-wrap-in-var [env f]
  (if (and (-> @cljs.env/*compiler* :options :optimizations (= :none))
           (symbol? f) (cljs-resolve env f))
    `(cljs.core/Var. (cljs.core/fn [] ~f) ~f nil)
    f))

(defn- on-calls [env tag ons]
  (let [static? (partial static? env)
        ons (if (handler? ons) [ons] ons)]
    (map (fn [[k f & args]]
           (let [property-name (as-on-property tag k)]
             (assert property-name (str (property-with-on k) " is not a property of " tag))
             (if (and (static? f) (every? static? args))
               (let [l (count args)]
                 (cond (= 0 l) `(on-static ~(as-on-property tag k) ~(maybe-wrap-in-var env f))
                       (= 1 l) `(on-static1 ~(as-on-property tag k) ~(maybe-wrap-in-var env f)
                                            ~(nth args 0))
                       (= 2 l) `(on-static2 ~(as-on-property tag k) ~(maybe-wrap-in-var env f)
                                            ~(nth args 0) ~(nth args 1))
                       :else `(on-static3 ~(as-on-property tag k) ~(maybe-wrap-in-var env f)
                                          ~(nth args 0)
                                          ~(nth args 1)
                                          ~(nth args 2))))
               (let [l (count args)]
                 (cond (= 0 l) `(on ~(as-on-property tag k) ~(maybe-wrap-in-var env f))
                       (= 1 l) `(on1 ~(as-on-property tag k) ~(maybe-wrap-in-var env f)
                                     ~(nth args 0))
                       (= 2 l) `(on2 ~(as-on-property tag k) ~(maybe-wrap-in-var env f)
                                     ~(nth args 0) ~(nth args 1))
                       :else `(on3 ~(as-on-property tag k) ~(maybe-wrap-in-var env f)
                                   ~(nth args 0)
                                   ~(nth args 1)
                                   ~(nth args 2)))))))
         ons)))

(defn- attribute-calls [env tag attrs]
  (reduce (fn [calls [k v]]
            (cond
              (= k ::key) calls
              (= k ::hooks) calls
              (= k :styleClass) (if (vector? v)
                                  (if (every? (partial static? env) v)
                                    (conj calls `(style-classes-static ~v))
                                    (conj calls `(style-classes ~v)))
                                  (if (static? env v)
                                    (conj calls `(style-class-static ~v))
                                    (conj calls `(style-class ~v))))
              (= k :style) (let [style-call (style-map->arr v)
                                 property-name (as-property tag :style)]
                             (assert property-name (str "style is not a property of " tag))
                             (if (every? (partial static? env) (rest style-call))
                               (conj calls `(style-static ~property-name ~style-call))
                               (conj calls `(style ~property-name ~style-call))))
              (= k ::on) (into calls (on-calls env tag v))
              (and (JUtils/isAssignableFromTextField tag) (= (name k) "text"))
              (conj calls (if (static? env v)
                            `(prop-static ~(as-property tag k) ~v)
                            `(input-value ~v)))
              :else (let [property-name (as-property tag k)]
                      (assert property-name (str (name k) " is not a property of " tag))
                      (conj calls (if (static? env v)
                                    `(prop-static ~property-name ~v)
                                    `(prop ~property-name ~v))))))
          '() attrs))

(defn- with-svg-namespace [tag body]
  (case tag
    "svg" `(do
             (set! *svg-namespace* (cljs.core/inc *svg-namespace*))
             ~@body
             (set! *svg-namespace* (cljs.core/dec *svg-namespace*)))
    ;; *svg-namespace* is set to 0 in open-impl
    "foreignObject" `(let [parent-svg-namespace# *svg-namespace*]
                       ~@body
                       (set! *svg-namespace* parent-svg-namespace#))
    `(do ~@body)))

(defn compile-element-macro
  [env tag typeid body]
  (let [compile-form (partial compile-form env)
        {key ::key
         {will-update :will-update will-unmount :will-unmount
          remove-hook :remove-hook
          did-mount :did-mount did-update :did-update} ::hooks :as attrs} (attributes body)
        _ (validate-attributes attrs)
        body (body-without-attributes body attrs)]
    (with-svg-namespace tag
      `((open ~tag ~typeid ~key ~will-update ~will-unmount ~remove-hook)
        ~@(attribute-calls env tag attrs)
        ~@(map compile-form body)
        (close ~did-mount ~did-update)))))

(defn- compile-text-macro
  [env tag typeid body]
  (let [{key ::key
         {will-update :will-update will-unmount :will-unmount
          remove-hook :remove-hook
          did-mount :did-mount did-update :did-update} ::hooks :as attrs} (attributes body)
        _ (validate-attributes attrs)
        body (body-without-attributes body attrs)]
    `(do
       (open ~tag ~typeid ~key ~will-update ~will-unmount ~remove-hook)
       ~@(attribute-calls env tag attrs)
       ~@(attribute-calls env tag {:text (apply str body)})
       (close ~did-mount ~did-update))))

(defmacro ^:private make-text-macro
  [name tag]
  `(defmacro ~(with-macro-meta name tag) [~'& ~'body]
     (swap! typeid inc-typeid)
     (compile-text-macro ~'&env ~(str tag) @typeid ~'body)))

(make-text-macro text javafx.scene.text.Text)

(defn- with-macro-meta [name tag]
  (with-meta name (assoc (meta name) ::tag (str tag))))

(defmacro make-element-macro
  "Defines a new HTML element macro with the provided tag. The newly defined HTML element macro
  can be used during a Muance vtree patching to create an HTML element which name is the provided
  tag."
  [name tag]
  `(defmacro ~(with-macro-meta name tag) [~'& ~'body]
     (swap! typeid inc-typeid)
     (compile-element-macro ~'&env ~(str tag) @typeid ~'body)))

(defn- params-with-props [params]
  (cond (symbol? params) [params params]
        (vector? params) (let [as-index (.indexOf params :as)
                               props-sym (if (not= -1 as-index)
                                           (get params (inc as-index))
                                           (gensym "props"))
                               params (if (= -1 as-index) (conj params :as props-sym) params)]
                           [params props-sym])
        (map? params) (let [props-sym (or (:as params) (gensym "props"))
                            params (if (:as params) params (conj params [:as props-sym]))]
                        [params props-sym])
        :else nil))

(comment
  (params-with-props '[1 2 :as kk])
  (params-with-props '{e :e :as ff})
  )

(defn- refresh-roots [repl-env compiler-env]
  (cljs.repl/-evaluate
   repl-env "<cljs repl>" 1
   "muance.core.refresh_roots();"))

;; The comp-fn does not need to be a var in order to support reloading because of the level
;; of indirection intriduced by variadic arity functions
;; Although it would be better for comp-fn to be a var to avoid relying on clojurescript inernals 
(defmacro defcomp
  "Define a Muance stateful component. A Muance component takes zero or one argument."
  [name docstring-or-params & params-body]
  (when (-> @cljs.env/*compiler* :options :optimizations (= :none))
    (swap! cljs.env/*compiler* update :replique/ns-watches
           assoc ana/*cljs-ns* refresh-roots))
  (swap! comp-typeid dec-comp-typeid)
  (let [typeid @comp-typeid
        name (if (string? docstring-or-params)
               (vary-meta name assoc :doc docstring-or-params)
               name)
        name (vary-meta name assoc ::component true)
        params (if (string? docstring-or-params) (first params-body) docstring-or-params)
        _ (assert (<= (count params) 1)
                  (str ana/*cljs-ns* "/" name " must take 0 or 1 parameter"))
        [params-with-props props-sym] (params-with-props (first params))
        body (if (string? docstring-or-params) (rest params-body) params-body)
        key-sym (gensym "key")]
    `(defn ~name
       ~(if params-with-props
          `([~props-sym]
            (~name nil ~props-sym))
          `([]
            (~name nil)))
       (~(if params-with-props [key-sym params-with-props] [key-sym])
        (cljs.core/let [parent-component# *component*
                        hooks# (goog.object/get comp-hooks ~(str ana/*cljs-ns* "/" name))]
          (open-comp ~(str ana/*cljs-ns* "/" name)
                     ~typeid ~(boolean params-with-props)
                     ~(when params-with-props props-sym)
                     ~name ~key-sym hooks#)
          (cljs.core/when-not *skip*
            ~@body)
          (close-comp parent-component# hooks#))))))

(defmacro hooks
  "Attaches a set of lifecycle hooks to a Muance component. hooks-map must be a literal map of
  lifecycle hooks."
  [component hooks-map]
  (let [not-a-comp-msg "muance.core/hooks first parameter must be a component"
        _ (assert (symbol? component) not-a-comp-msg)
        resolved (cljs-resolve &env component)
        comp-ns (and resolved (symbol (namespace resolved)))
        comp-sym (and resolved (symbol (name resolved)))
        _ (assert (and comp-ns comp-sym) not-a-comp-msg)
        var-map (get-in @cljs.env/*compiler* [::ana/namespaces comp-ns :defs comp-sym])]
    (assert (get-in var-map [:meta ::component]) not-a-comp-msg)
    (assert (map? hooks-map))
    (let [{will-update :will-update will-unmount :will-unmount
           remove-hook :remove-hook
           did-mount :did-mount did-update :did-update
           will-receive-props :will-receive-props
           get-initial-state :get-initial-state :as attrs} hooks-map]
      `(goog.object/set
        comp-hooks
        ~(str comp-ns "/" comp-sym)
        (cljs.core/array ~get-initial-state ~will-receive-props
                         ~did-mount ~did-update ~will-unmount
                         ~remove-hook ~will-update)))))

(defmacro run-later [& body]
  `(let [runnable# (js/Java.extend muancefx.core/Runnable
                                   (cljs.core/js-obj "run" (cljs.core/fn [] ~@body)))]
     (.runLater muancefx.core/Platform (new runnable#))))

;; call a muancefx compile-element-macro from muancefx.h
;; no svg
;; custom attribute-calls
