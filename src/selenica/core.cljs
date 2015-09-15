(ns ^:figwheel-always selenica.core
  (:require-macros
    [selenica.macros :refer [mapf ? ..! each]]
    [heh.core :refer [html component]])
  (:require
    [clojure.string :as string]
    [om.core :as om :include-macros true]
    [om.dom :as dom :include-macros true]
    [heh.core :refer [private private! emit! down!]]
    [dollar.bill :as $ :refer [$]]
    [cognitect.transit :as transit :refer [writer reader read]]
    [selenica.data :as DATA]))

(enable-console-print!)


(defn inject-css [id s]
  (let [el (or (first ($ (str "#" id)))
               (first ($ (str "<style id='" id "'></style>"))))]
    (aset el "innerHTML" s)
    (.appendChild (first ($ "head")) el)))

 
(defn get-file [url f fail]
  (when-let [req (new js/XMLHttpRequest)]
    (set! (.-onreadystatechange req)
      (fn [e] 
        (if (= 4 (.-readyState req))
          (if (= 200 (.-status req)) 
            (f (.. e -target -response))
            (when fail (fail e))))))
    (.open req "GET" url false)
    (.overrideMimeType req "text/xml; charset=iso-8859-1")
    ;(set! (.-responseType req) "arraybuffer")
    (.send req)))


 
 
(defonce  app-state (atom 
{:splash {:idx 0}
 :context []
 :touch {:velocity [0 0]}
 :view {:screen :splash}
 :inventory DATA/INVENTORY

  }))
 
(def splash-moons
  (for [f (identity ["001" "002" "003" "004" "005"])]
    (str "img/splash/" f ".png")))

(def books
   (reverse (reduce 
  #(cons (assoc %2 :left 
      (+ 5 (:left (first %1)) (:width (first %1)))) %1) 
  (for [i (range 80)]
    {:width (+ 15 (rand-int 20)) 
      :height (+ 105 (rand-int 20)) 
     :color (rand-nth ["brown" "tomato" "purple" "gray" "blue"])}))))



(defn make-img [path]
  (let [img (.createElement js/document "img")]
    (aset img "src" path )
    img))

(def drag-cursor (make-img "img/dragcursor.png"))

(component searchbox [data owner opts]
  (render-state [_ state]
    (html (<label "search" (<input)))))

(defn do-drag-start [e owner]
  (let [[x y] [(.-clientX e)(.-clientY e)]]
    (.setDragImage (.-dataTransfer e) drag-cursor 16 16)
    (.setData (.-dataTransfer e) "Text" "books")
    (aset (.-dataTransfer e) "effectAllowed" "move")

    (private! owner :dragstart [x y])
    (private! owner :draglast [x y])
    (inject-css "force" "body{cursor:move !important;} ")
    (.stopPropagation e)))

(defn do-drag [e owner]
  (let [[x y] [(.-clientX e)(.-clientY e)]
        [dx dy] (mapv - [x y] (private owner :draglast) )]
        (.preventDefault e)
    (if (= [0 0] [x y])
      [0 0]
      (do (private! owner :draglast [x y])
          [dx dy]))))

(defn do-drag-end [e owner]
  (let [[x y] [(.-clientX e)(.-clientY e)]]
    (private! owner :dragstart [x y])
    (private! owner :draglast [x y])
    (inject-css "force" "")
    (.stopPropagation e)))



(component book-scroller [data owner opts]
  (render-state [_ state]
    (html
      (<div.footer
        (<div.scroller.horizontal
          (ref "scroller") 
          (draggable true)
          (onDragStart (fn [e] (do-drag-start e owner)))
          (onDragEnd (fn [e] (do-drag-end e owner)))
          (onDrag 
            (fn [e] 
              (let [[x y] (do-drag e owner)
                    el (om/get-node owner "scroller")
                    str-x (aget (.-style el) "left")
                    el-x (if (= "" str-x) 0 (js/parseInt str-x))]
                (aset (.-style el) "left" (str (+ el-x x) "px")))))
          (into-array (map 
            #(<div.book 
              (style {
                :background (:color %) 
                :left (:left %)
                :width (:width %)
                :height (:height %) }))
            books)))))))

;(:description :date :publisher :observations :dimensions :title :author :language :id 
;  :condition :notes :materials :provenance :domain :id-prov :links)

(component listing [data owner opts]
  (render-state [_ state]
    (html
      (<div.listing
        (<div.meta 
          (<span.date (:date data))
          (<span.form (str (:domain data) " - " (:language data)))
          (<span.dimensions 
            (prn-str (:dimensions data)))
          
          (<span.condition (:condition data) (class (:condition data) ))
          (<span.id (:id data)))
        (<div.nouns 
          (<span.title (:title data))
          (<span.author (:author data)))
        (<div.details 
          (into-array (map
            #(let [
              english (get-in DATA/english [(:id data) (keyword %)])
              french (get data (keyword %))]
              (when (or english french)
                (<div (<label (str % ": ")) 
                  (<span (class (if english "english" "french")) 
                    (or english french)))))
            ['description 'publisher 'observations 'notes 'materials 'provenance 'links]))
 ))))) 
 
(def book-sorts {
  :date #(if (string? (:date %)) 9999999 (:date %))
  :biggest #(if (and (vector? (:dimensions %)) (> 1 (count (:dimensions %)))) 
                (let [[w h] (:dimensions %)] (* w h)))})

(def book-filters {
  :valid-date #(number? (:date %))
  :map #(re-find #"c|Cartes" (:domain %))
  :french #(= (:language %) "Français")})
 
(component inventory [data owner opts]
  (render-state [_ state]
    (html
      (<div#inventory 
          (ref "scroller") 
          (draggable true)
          (style {:position :absolute :overflow :hidden})
          (onDragStart (fn [e] (do-drag-start e owner)))
          (onDragEnd (fn [e] (do-drag-end e owner)))
          (onDrag 
            (fn [e] 
              (let [[x y] (do-drag e owner)
                    el (om/get-node owner "scroller")
                    str-x (aget (.-style el) "top")
                    el-x (if (= "" str-x) 0 (js/parseInt str-x))]
                (aset (.-style el) "top" (str (+ el-x y) "px")))))
        (into-array (map #(om/build listing % {}) 
          (sort-by (:date book-sorts)
            (filter 
              (every-pred  
                (:valid-date book-filters)
                (:map book-filters)
                ;(complement (:french book-filters))
                )
            (vals (:inventory data))))))))))

(component splash [data owner opts]
  (render-state [_ state]
    (html
      (<div#splash 
          (onClick (fn [e] (om/transact! data [:splash :idx] inc)))
          (into-array (map-indexed 
            #(<img (src %2)
              (style {:opacity 
                (if (<= (- 5 %1) (:idx (:splash data)))
                  0.0 1.0)}))
            splash-moons))
          (<h1 "Cabinet Selenica")
          (<button "inventory"
            (onClick #(om/update! data [:view] {:screen :inventory})))
          (om/build searchbox data {}))
        (om/build book-scroller data {}))))

(def views {:splash splash :inventory inventory})

(component main [data owner opts]
  (render-state [_ state]
    (html
      (<div#main
        (om/build (get views (:screen (:view data))) data {})))))


 
(om/root main app-state
  {:target (. js/document (getElementById "app"))})


(defn on-js-reload []

)


(comment   
(prn (identity (mapv (fn [[k v]] 
  [k (vec (remove nil? (map #(when-let [b (get v %1)] [%2 b]) 
    [:description :observations :notes :materials][-1 -2 -3 -4])))]) DATA/INVENTORY)))

(prn (into {} 
  (mapv (fn [[k vs]]
    [k (into {} 
      (map (juxt (comp {-1 :description -2 :observations -3 :notes -4 :materials} first)
              last)
       vs))]) 
    DATA/english)))
)

 
(defn ->edn [o] (transit/write (transit/writer :json) o))
(defn edn-> [s] (transit/read (transit/reader :json) s))

(def INV (atom nil))

(declare parse-csv-cells float-str? int-str?)

(defn parse-csv-cells [s] (map (comp #(string/trim %) #(string/replace % #"[\"]" "") first)  
  (re-seq #"([\"]([^\"]+)[\"])|,(,)|([^,\"]+)" (string/replace s #"\"\"" "'"))))

(defn parse-french-dimensions [s] 
  (mapv js/parseFloat (string/split (string/replace s #"," ".") #"\W+x\W+" )))

(defn remove-nil-kv [m]
  (into {} (filter #(not (nil? (last %))) m)))

(defn alter-inventory-kvs [m]
    (if (string? (:dimensions m))
      (update-in m [:dimensions] parse-french-dimensions)
      m))
 
(defn float-str? [s] (re-seq #"^[1-9][0-9]*\.[0-9]*$" s))
(defn int-str? [s] (re-seq #"^[1-9][0-9]*$" s))

(defn cell-cast [s]
  (cond (not (string? s)) s
        (#{",," " " ""} s) nil 
        (float-str? s) (js/parseFloat s)
        (int-str? s) (js/parseInt s)
        :else s))

(defn parse-csv [buffer]
  (let [rows (string/split buffer #"\n")
        schema (mapv keyword 
                (take-while #(not= "" %) 
                  (string/split (first rows) ",")))
        len (count schema)
        data (map #(zipmap schema (map cell-cast (take len (parse-csv-cells %)))) (rest rows))
        edn (into {} (map #(vector (:id %) (alter-inventory-kvs (remove-nil-kv %))) data))]
        (prn edn)
        edn
        )) 

 
(comment 
(get-file "./data/inventory.csv" 
  #(do (reset! INV (parse-csv %))) 
  #(prn "fail to load xls"))


(get-file "./data/inventory.edn" 
  #(do ;(prn (edn-> %))
    ) 
  #(prn "fail to load xls"))


  )