(ns quiescent.dom)

(defn- component-fn-symbol [js-namespace tag]
  (symbol (str "js/" js-namespace "." (name tag))))

(defn- tag-definition-base [f tag]
  `(defn ~tag [& args#]
     ~(str "Return a component for " tag)
     (let [a# (make-array 0)]
       (.push a# (cljs.core/clj->js (first args#)))
       (doseq [arg# (rest args#)] (.push a# arg#))
       (.apply ~f nil a#))))

(defn tag-definition
  "Return a form to defining a wrapper function for a ReactJS tag
  component."
  ([tag]
   (tag-definition-base (component-fn-symbol "React" tag) tag))
  ([js-namespace tag]
   (tag-definition-base `(.createFactory js/React ~(component-fn-symbol js-namespace tag)) tag)))

(defn- define-tags-base [mapping-fn tags]
  `(do (do ~@(clojure.core/map mapping-fn tags))
     (def ~'defined-tags
       ~(zipmap (map (comp keyword name) tags)
                tags))))

(defmacro define-tags
  "Macro which expands to a do block which contains a defmacro for
  each supported HTML and SVG tag. The resulting macros take
  an (optional) properties argument, and any number of child
  arguments. The properties argument may be a Clojure map or a JS
  object."
  [& tags]
  (define-tags-base tag-definition tags))

(defmacro define-composite-tags [js-namespace & tags]
  (define-tags-base #(tag-definition js-namespace %) tags))
