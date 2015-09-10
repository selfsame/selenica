 (ns selenica.macros)

(defmacro column ([n & b]
                  (let [width (if (number? n) (str n "%") "100%")
                        body (if (number? n) b (cons n b)) ]
                       `(~'dom/div (~'clj->js {:className "ui-column"
                                               :style (~'clj->js {:width ~width}) }) ~@body))))

(defmacro row
          ([& body]
           `(~'dom/div (~'clj->js {:className "ui-row clearfix"}) ~@body)))

(defmacro label
          ([& body]
           `(~'dom/p (~'clj->js {:className "ui-label"}) ~@body)))

(defmacro group
          ([& body]
           `(~'dom/div (~'clj->js {:className "ui-group clearfix"}) ~@body)))





(defmacro render-count [& more]
          (when (even? (count more))
                (let [sh-m (mapv #(case %
                                        :bg :background-color :t :top :l :left :r :right :b :bottom %) more)
                      m (apply hash-map sh-m)]
                     `(~'when ~'doc-ref
                       (~'when-let [~'rcopt (~'doc-ref :options :debug :render-count)]
                        (~'when (:value ~'rcopt)
                         (~'let [~'rc (~'or (~'plomb/private ~'owner :rc) 0)]
                          (~'plomb/private! ~'owner :rc (~'inc ~'rc))
                          (~'dom/div (~'clj->js {:className "render-counter"
                                                 :style (~'clj->js ~m)}) (~'str ~'rc))
                          )))))))




(defmacro mapf [args & body]
          (let [fbody (first body)
                more (rest body)]
               `(~'mapv (~'fn ~args ~fbody) ~@more)))

(defmacro owned [t]
          (let [tag (cond (string? t) t
                          (keyword? t) (apply str (rest (str t))))]
               `(~'om/get-node ~'owner ~tag)))

(defmacro state! [& body]
          `(~'om/set-state! ~'owner ~@body))

(defmacro when= [a b & body]
          `(~'when (~'= ~a ~b) ~@body))



(defmacro ? [& body]
          (let [[conds _ elses] (partition-by #(not= :else %) body)]
               (if elses
                `(~'cond ~@conds :else (~'do ~@elses))
                `(~'cond ~@conds))))
(var? #'?)


(defn prop-symbol [k]
      (cond (string? k) (symbol (str ".-" k))
            (keyword? k) (symbol (apply str (cons ".-" (rest (str k)))))
            (symbol? k) (symbol (str ".-" k))
            :else k))

(defmacro ..! [& body]
          (let [value (last body)
                object (first body)
                path (map prop-symbol (butlast (rest body)))
                access (reduce (fn [form part] (list part form)) object path)]
               `(~'set! ~access ~value)))

(defmacro each [bound col & body]
          `(~'for [~bound ~col] (~'do ~@body)))





(defmacro sandwich [col args & code]
          "[col [prev current next index] code]"
          (let []
               `(remove nil? (apply concat (map-indexed
                                            (fn [i# v#]
                                                (let [~args [(get ~col (dec i#)) v# (get ~col (inc i#)) i#]]
                                                     ~@code))
                                            ~col)))))






(defn attr-merge [a b]
      (? (string? b) b
         (vector? b) (vec (concat a b))
         (sequential? b) (concat a b)
         :else (conj a b)))

(defn attr-post [m]
      (? (empty? (:class m)) (dissoc m :class)
         :else (update-in m [:class] #(list 'apply 'str (list 'interpose " " %));#(list ('apply 'str ('interpose " " %)))
                          )))

(defn style-js [m]
      (? (map? (:style m))
         (update-in m [:style] #(list 'clj->js %))
         :else m))




(defn meta-use [form]
      (let [mm (mapv #(:_html (meta %)) form)
            res (if (first mm)
                 (concat [(first form)
                          (attr-post (apply merge-with attr-merge (filter map? mm)))]
                         (filter #(not= '_nil %) (rest form)))
                 form)]
           res))





(def -attrs #{'onTouchMove 'onKeyDown 'onLoad 'onLoadedData 'onInput 'onChange 'onDragStart 'onDrop
              'onContextMenu 'onFocus 'href 'onClick 'onMouseUp 'onCut 'onMouseOut 'onTouchEnd 'onDrag
              'onDragExit 'ref 'onDragEnter 'onTouchCancel 'value 'onSubmit 'onMouseDown 'onMouseEnter
              'type 'src 'onTouchStart 'onBlur 'onWheel 'onPaste 'onMouseMove 'onHover 'style 'onKeyUp
              'onMouseOver 'onKeyPress 'onDragOver 'onDragLeave 'draggable 'onDoubleClick 'id 'class
              'checked 'onMouseLeave 'onDragEnd 'onCopy 'onScroll})

(defn -attr [form]
      (let [attr (first form)
            value (first (rest form))]
           (? (= 'class attr) (vec (rest form))
              :else value)))


(defn soup [s]
      (let [[_ tag] (re-find #"\<([\w-_]+)" s)
            [_ id] (re-find #"\#([\w-_]+)" s)
            mchr (re-matcher #"\.([\w-_]+)" s)
            classes (vec (filter string? (each _ (range 10) (last (re-find mchr)))))
            mta (conj {} (? id {:id id}) (if-not (empty? classes) {:class classes}))]
           (when tag
                 (with-meta
                  (symbol (str "el/" tag))
                  {:_html mta}))))

(defn html-recur [form]
      (? (symbol? form) (or (soup (str form)) form)
         (vector? form) (meta-use (mapv html-recur form))
         (sequential? form) (? (-attrs (first form))
                               (let [res {(keyword (first form)) (-attr form)}]
                                    (with-meta '_nil {:_html res}))
                               :else (meta-use (map html-recur form)))
         :else form))

(defmacro html [& body]
          (let [forms (html-recur body)]
               `(do ~@forms)))


(macroexpand
 '(html
   (<div (list (<span) (<ul)))))



(def ^:private component-protocols
 {'display-name       `om/IDisplayName
  'init-state         `om/IInitState
  'should-update      `om/IShouldUpdate
  'will-mount         `om/IWillMount
  'did-mount          `om/IDidMount
  'will-unmount       `om/IWillUnmount
  'will-update        `om/IWillUpdate
  'did-update         `om/IDidUpdate
  'will-receive-props `om/IWillReceiveProps
  'render             `om/IRender
  'render-state       `om/IRenderState})


(defmacro component [sym args & code]
          (let [sym-str (str sym)
                pcols (mapcat #(if-let [p (component-protocols (first %))] [p %] []) code)]
               `(defn ~sym ~args
                      (reify
                       ~'om/IDisplayName
                       (~'display-name [~'_] ~sym-str)
                       ~@pcols
                       ))))

(defmacro -el [tag opts & more]
          (let [ctor (symbol (str "dom/" tag))
                jsmap (if (map? opts) (list 'clj->js opts) nil)
                inner (if jsmap more (cons opts more))
                children (flatten inner)]
               `(~ctor ~jsmap ~@children)))