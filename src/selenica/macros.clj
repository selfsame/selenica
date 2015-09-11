 (ns selenica.macros)

(defmacro column 
  ([n & b]
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
   `(remove nil? (apply concat 
    (map-indexed
      (fn [i# v#]
          (let [~args [(get ~col (dec i#)) v# (get ~col (inc i#)) i#]]
               ~@code))
      ~col)))))


