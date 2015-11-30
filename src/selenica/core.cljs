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

(defn ->edn [o] (transit/write (transit/writer :json) o))
(defn edn-> [s] (transit/read (transit/reader :json) s))
(defn put-local [k v] (.setItem js/localStorage k v))
(defn get-local [k] (.getItem js/localStorage k ))

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

(def schema { 
  :description  :long
  :date         :integer
  :city         :short
  :dimensions   {:vector true :form [:number :number :number]}
  :title        :short
  :author       :short
  :language     :short
  :id           :short
  :condition    :short
  :notes        :long
  :materials    :long
  :provenance   :long
  :domain       :short
  :id-prov      :short
  :images       {:vector true :form :short}
  })
 
 
(defonce  app-state (atom 
{:splash {:idx 0}
 :context []
 :touch {:velocity [0 0]}
 :view {:screen :splash}
 :inventory (edn-> (get-local "inventory")) ;DATA/INVENTORY

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


(component vec-editor [data owner opts]
  (render-state [_ state]
    (let [scheme (get schema opts)]
    (html
      (<div.vector
        (if (vector? (:form scheme))
          (<button "+"
            (onClick (fn [_] (om/transact! data #(conj % ""))))))
        (<hr)
          (into-array (map 
            (fn [v]
              (let []
                (<input (value (str v)))))
            data)))))))
 

(component listing-editor [data owner opts]
  (render-state [_ state]
    (html
      (<div.editing
          (into-array (map 
            (fn [[k v]]
              (let [scheme (get schema k)
                    key-str (.-name k)]
                (<label 
                  (ref key-str)
                  (class (str key-str)) 
                  key-str
                  (cond (:vector scheme)
                        (om/build vec-editor (get data k) {:opts {:schema scheme}})
                        (#{:short :number :integer} scheme)
                        (<input (value (str v)))
                        (#{:long} scheme)
                        (<textarea (value (str v)))))))
            (sort-by #({:short -1 :long 5} 
              (get schema (first %))) data)))))))

(def book-sorts {
  :date #(if (string? (:date %)) 9999999 (:date %))
  :biggest #(if (and (vector? (:dimensions %)) (> 1 (count (:dimensions %)))) 
                (let [[w h] (:dimensions %)] (* w h)))})

(def book-filters {
  :valid-date #(number? (:date %))
  :map #(re-find #"c|Cartes" (:domain %))
  :french #(= (:language %) "Français")})

(component listing [data owner opts]
  (render-state [_ state]
    (html
      (if (:editing state) 
        (om/build listing-editor data {})
        (<div.listing
          (<div.meta 
            (<span.date (:date data))
            (<span.form (str (:domain data) " - " (:language data)))
            (<span.dimensions 
              (prn-str (:dimensions data)))
            (if (:editing state)
              (<span 
                (<button "save"
                  (onClick #(om/update-state! owner :editing not)))
                (<button "discard"
                  (onClick #(om/update-state! owner :editing not))))
              (<span
                (<button "edit"
                  (onClick #(om/update-state! owner :editing not)))))
            (<span.condition (:condition data) (class (:condition data) ))
            (<span.id (:id data)))
          (<div.nouns 
            (<span.title (:title data))
            (<span.author (:author data))
            (<img (src (str "img/" (rand-nth DATA/IMAGES)))))

          (<div.details 
            (into-array (map
              #(let [
                english (get-in DATA/english [(:id data) (keyword %)])
                french (get data (keyword %))]
                (when (or english french)
                  (<div (<label (str % ": ")) 
                    (<span (class (if english "english" "french")) 
                      (or english french)))))
              ['description 'publisher 'observations 'notes 'materials 'provenance 'links]))))))))
   

(component searchbox [data owner opts]
  (render-state [_ state]
    (html (<label "search" (<input)))))

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
                ;(:french book-filters)
                ;(:map book-filters)
                ;(complement (:french book-filters))
                )
            (vals (:inventory data))))))))))

(component splash [data owner opts]
  (render-state [_ state]
    (html
      (<div#splash 
          (<h1 "Cabinet Selenica")
          (<button "inventory"
            (onClick #(om/update! data [:view] {:screen :inventory})))
          (om/build searchbox data {})))))

(def views {:splash splash :inventory inventory})

(component main [data owner opts]
  (render-state [_ state]
    (html
      (<div#main
        (om/build (get views (:screen (:view data))) data {})))))


 
(om/root main app-state
  {:target (. js/document (getElementById "app"))})


(defn on-js-reload [])