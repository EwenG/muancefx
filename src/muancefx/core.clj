(ns muancefx.core
  (:require [clojure.string :as string]
            [muance.core :as m]
            [cljs.analyzer :as ana]
            [cljs.repl])
  (:import [cljs.tagged_literals JSValue]
           [muancefx JUtils]
           [clojure.lang RT]
           [java.beans PropertyDescriptor]))

(declare compile-element-macro)
(declare text)

(def ^:const local-state-sym 'muancefx.core/*state*)

(defn- compile-form [env form]
  (cond (and (seq? form) (symbol? (first form)))
        (let [var (#'m/cljs-resolve env (first form))
              clj-var (resolve var)]
          (cond (::tag (meta clj-var))
                (compile-element-macro env
                                       (::tag (meta clj-var))
                                       nil
                                       (::children-getter (meta clj-var))
                                       (::max-children (meta clj-var))
                                       (rest form))
                :else form))
        (string? form) (compile-element-macro env "javafx.scene.text.Text" nil
                                              "getChildren" 0 `(:text ~form))
        :else form))

;; Like clojure.string/capitalize but do not lowercase other letters
(defn- ^String capitalize [^CharSequence s]
  (let [s (.toString s)]
    (if (< (count s) 2)
      (.toUpperCase s)
      (str (.toUpperCase (subs s 0 1)) (subs s 1)))))

(defn- property-with-prefix [prefix property]
  (->> (name property) capitalize (str prefix)))

(defn- match-property-method? [name params-count method]
  (and (= (.getName method) name)
       (= params-count (count (.getParameterTypes method)))))

(defn- property-method [class-name property-method-name params-count]
  (let [method (->> (.getMethods
                     (Class/forName class-name false (RT/baseLoader)))
                    (filter (partial match-property-method?
                                     property-method-name params-count))
                    first)]
    (when method
      {:name (.getName method)
       :params (.getParameterTypes method)})))

(comment
  (import '[javafx.scene.control.ListView])
  (.getClass javafx.scene.control.ListView)

  (->> (Class/forName "javafx.scene.control.ListView" false (RT/baseLoader))
       (.getMethods)
       (map #(.getParameterTypes %))
       (into [])
       (take 4))

  (->> (Class/forName "javafx.scene.control.ListView" false (RT/baseLoader))
       (.getMethods)
       (filter #(= (.getName %) "setItems"))
       first
       (.getParameterTypes)
       count
       #_(into []))
  
  )

(defn- as-property [class-name property]
  (let [property-method-name (str (name property) "Property")]
    (property-method class-name property-method-name 0)))

(defn- as-property-setter [class-name property]
  (let [property-method-name (property-with-prefix "set" property)]
    (property-method class-name property-method-name 1)))

(defn- as-on-property-setter [class-name property]
  (let [property-method-name (property-with-prefix "setOn" property)]
    (property-method class-name property-method-name 1)))

(comment
  (as-property-setter "javafx.scene.control.TextField" "onAction")
  (as-on-property-setter "javafx.scene.control.TextField" :action)

  (as-property-setter "javafx.scene.control.Button" :onMouseClicked)
  (as-on-property-setter "javafx.scene.control.Button" :mouseClicked)
  )

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

(defn- on-calls [env tag ons]
  (let [static? (partial #'m/static? env #{local-state-sym})
        ons (if (#'m/handler? ons) [ons] ons)]
    (map (fn [[k f & args]]
           (let [property-name (:name (as-on-property-setter tag k))]
             (assert property-name (str (property-with-prefix "setOn" k) " is not a property of " tag))
             (if (and (static? f) (every? static? args))
               (let [l (count args)]
                 (cond (= 0 l) `(on-static ~property-name
                                           ~(#'m/maybe-wrap-in-var env f))
                       (= 1 l) `(on-static1 ~property-name
                                            ~(#'m/maybe-wrap-in-var env f)
                                            ~(nth args 0))
                       (= 2 l) `(on-static2 ~property-name
                                            ~(#'m/maybe-wrap-in-var env f)
                                            ~(nth args 0) ~(nth args 1))
                       :else `(on-static3 ~property-name
                                          ~(#'m/maybe-wrap-in-var env f)
                                          ~(nth args 0)
                                          ~(nth args 1)
                                          ~(nth args 2))))
               (let [l (count args)]
                 (cond (= 0 l) `(on ~property-name
                                    ~(#'m/maybe-wrap-in-var env f))
                       (= 1 l) `(on1 ~property-name
                                     ~(#'m/maybe-wrap-in-var env f)
                                     ~(nth args 0))
                       (= 2 l) `(on2 ~property-name
                                     ~(#'m/maybe-wrap-in-var env f)
                                     ~(nth args 0) ~(nth args 1))
                       :else `(on3 ~property-name
                                   ~(#'m/maybe-wrap-in-var env f)
                                   ~(nth args 0)
                                   ~(nth args 1)
                                   ~(nth args 2)))))))
         ons)))

(defn- listen-calls [env tag listeners]
  (let [static? (partial #'m/static? env #{local-state-sym})
        listeners (if (#'m/handler? listeners) [listeners] listeners)]
    (map (fn [[k f & args]]
           (let [property-name (:name (as-property tag k))]
             (assert property-name (str (name k) " is not a property of " tag))
             (if (and (static? f) (every? static? args))
               (let [l (count args)]
                 (cond (= 0 l) `(listen-static ~property-name
                                               ~(#'m/maybe-wrap-in-var env f))
                       (= 1 l) `(listen-static1 ~property-name
                                                ~(#'m/maybe-wrap-in-var env f)
                                                ~(nth args 0))
                       (= 2 l) `(listen-static2 ~property-name
                                                ~(#'m/maybe-wrap-in-var env f)
                                                ~(nth args 0) ~(nth args 1))
                       :else `(listen-static3 ~property-name
                                              ~(#'m/maybe-wrap-in-var env f)
                                              ~(nth args 0)
                                              ~(nth args 1)
                                              ~(nth args 2))))
               (let [l (count args)]
                 (cond (= 0 l) `(listen ~property-name
                                        ~(#'m/maybe-wrap-in-var env f))
                       (= 1 l) `(listen1 ~property-name
                                         ~(#'m/maybe-wrap-in-var env f)
                                         ~(nth args 0))
                       (= 2 l) `(listen2 ~property-name
                                         ~(#'m/maybe-wrap-in-var env f)
                                         ~(nth args 0) ~(nth args 1))
                       :else `(listen3 ~property-name
                                       ~(#'m/maybe-wrap-in-var env f)
                                       ~(nth args 0)
                                       ~(nth args 1)
                                       ~(nth args 2)))))))
         listeners)))

(defmulti maybe-cast-param (fn [env property-name param-type property-val]
                             [property-name param-type]))

(defmethod maybe-cast-param ["setItems" javafx.collections.ObservableList]
  [env property-name param-type property-val]
  `(seqable->observable-list ~property-val))

(defmethod maybe-cast-param ["setCellFactory" javafx.util.Callback]
  [env property-name param-type property-val]
  (if-let [resolved (and (symbol? property-val) (#'m/cljs-resolve env property-val))]
    (let [comp-ns (symbol (namespace resolved))
          comp-sym (symbol (name resolved))
          var-map (get-in @cljs.env/*compiler* [::ana/namespaces comp-ns :defs comp-sym])]
      (if (get-in var-map [:meta ::component])
        `(fn->factory ~(#'m/maybe-wrap-in-var env property-val))
        property-val))
    property-val))

(defmethod maybe-cast-param :default [env property-name param-type property-val]
  property-val)

(defn- attribute-calls [env tag attrs]
  (reduce (fn [calls [k v]]
            (cond
              (= k ::key) calls
              (= k ::hooks) calls
              (= k :styleClass) (if (vector? v)
                                  (if (every? (partial #'m/static? env #{local-state-sym}) v)
                                    (conj calls `(style-classes-static ~v))
                                    (conj calls `(style-classes ~v)))
                                  (if (#'m/static? env #{local-state-sym} v)
                                    (conj calls `(style-class-static ~v))
                                    (conj calls `(style-class ~v))))
              (= k :style) (let [style-call (style-map->arr v)
                                 property-name (:name (as-property-setter tag :style))]
                             (assert property-name (str "style is not a property of " tag))
                             (if (every? (partial #'m/static? env #{local-state-sym})
                                         (rest style-call))
                               (conj calls `(style-static ~property-name ~style-call))
                               (conj calls `(style ~property-name ~style-call))))
              (= k ::on) (into calls (on-calls env tag v))
              (= k ::listen) (into calls (listen-calls env tag v))
              (and (JUtils/isAssignableFromTextField tag) (= (name k) "text"))
              (conj calls (if (#'m/static? env #{local-state-sym} v)
                            `(prop-static ~(:name (as-property-setter tag k)) ~v)
                            `(input-value ~v)))
              :else (let [{property-name :name [param-type] :params} (as-property-setter tag k)]
                      (assert property-name (str (name k) " is not a property of " tag))
                      (conj calls (if (#'m/static? env #{local-state-sym} v)
                                    `(prop-static
                                      ~property-name
                                      ~(maybe-cast-param env property-name param-type v))
                                    `(prop
                                      ~property-name
                                      ~(maybe-cast-param env property-name param-type v)))))))
          '() attrs))

(defn compile-element-macro
  [env tag typeid children-getter max-children body]
  (let [compile-form (partial compile-form env)
        {key ::key
         {will-update :will-update will-unmount :will-unmount
          remove-hook :remove-hook
          did-mount :did-mount did-update :did-update} ::hooks :as attrs} (#'m/attributes body)
        _ (#'m/validate-attributes attrs)
        body (#'m/body-without-attributes body attrs)]
    (assert (or (nil? max-children) (<= (count body) max-children))
            (str "Too many children for " tag))
    `(let [prev-children-getter# *children-getter*]
       (open ~tag ~typeid ~key ~will-update ~will-unmount ~remove-hook)
       (set! *children-getter* ~children-getter)
       ~@(attribute-calls env tag attrs)
       ~@(map compile-form body)
       (close ~did-mount ~did-update)
       (set! *children-getter* prev-children-getter#))))

(defmacro make-element-macro
  "Defines a new HTML element macro with the provided tag. The newly defined HTML element macro
  can be used during a Muance vtree patching to create an HTML element which name is the provided
  tag."
  [name tag children-getter max-children]
  `(defmacro ~(vary-meta name assoc
                         ::tag (str tag)
                         ::children-getter (str children-getter)
                         ::max-children max-children)
     [~'& ~'body]
     (swap! @#'m/typeid #'m/inc-typeid)
     (compile-element-macro ~'&env ~(str tag) @@#'m/typeid
                            ~(str children-getter) ~max-children ~'body)))

(defn- refresh-roots [repl-env compiler-env]
  (cljs.repl/-evaluate
   repl-env "<cljs repl>" 1
   "muancefx.core.refresh_roots();"))

;; The comp-fn does not need to be a var in order to support reloading because of the level
;; of indirection introduced by variadic arity functions
;; Although it would be better for comp-fn to be a var to avoid relying on clojurescript inernals
(defmacro defcomp
  "Define a Muancefx stateful component. A Muancefx component takes zero or one argument."
  [name docstring-or-params & params-body]
  (when (-> @cljs.env/*compiler* :options :optimizations (= :none))
    (swap! cljs.env/*compiler* update :replique/ns-watches
           assoc ana/*cljs-ns* refresh-roots))
  (swap! @#'m/comp-typeid #'m/dec-comp-typeid)
  (let [typeid @@#'m/comp-typeid
        name (if (string? docstring-or-params)
               (vary-meta name assoc :doc docstring-or-params)
               name)
        name (vary-meta name assoc ::component true)
        params (if (string? docstring-or-params) (first params-body) docstring-or-params)
        _ (assert (<= (count params) 1)
                  (str ana/*cljs-ns* "/" name " must take 0 or 1 parameter"))
        [params-with-props props-sym] (#'m/params-with-props (first params))
        body (if (string? docstring-or-params) (rest params-body) params-body)
        key-sym (gensym "key")]
    `(defn ~name
       ~(if params-with-props
          `([~props-sym]
            (~name nil ~props-sym))
          `([]
            (~name nil)))
       (~(if params-with-props [key-sym params-with-props] [key-sym])
        (cljs.core/let [parent-component# m/*component*
                        hooks# (goog.object/get m/comp-hooks ~(str ana/*cljs-ns* "/" name))]
          (open-comp ~(str ana/*cljs-ns* "/" name)
                     ~typeid ~(boolean params-with-props)
                     ~(when params-with-props props-sym)
                     ~name ~key-sym hooks#)
          (cljs.core/when-not m/*skip*
            ~@body)
          (close-comp parent-component# hooks#))))))

(defmacro hooks
  "Attaches a set of lifecycle hooks to a Muance component. hooks-map must be a literal map of
  lifecycle hooks."
  [component hooks-map]
  (let [not-a-comp-msg "muancefx.core/hooks first parameter must be a component"
        _ (assert (symbol? component) not-a-comp-msg)
        resolved (#'m/cljs-resolve &env component)
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
        m/comp-hooks
        ~(str comp-ns "/" comp-sym)
        (cljs.core/array ~get-initial-state ~will-receive-props
                         ~did-mount ~did-update ~will-unmount
                         ~remove-hook ~will-update)))))

(defmacro run-later [& body]
  `(async-fn (cljs.core/fn [] ~@body)))

;; more h macros
;; remove core things
;; css reloading
