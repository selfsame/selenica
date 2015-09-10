(ns ^:figwheel-always selenica.core
  (:require-macros
    [selenica.macros :refer [mapf ? ..! each]]
    [heh.core :refer [html component]])
  (:require
    [om.core :as om :include-macros true]
    [om.dom :as dom :include-macros true]
    [heh.core :refer [private private! emit! down!]]
    [dollar.bill :as $ :refer [$]]))

(enable-console-print!)



(def PRIVATE (atom {}))


(defn inject-css [id s]
  (let [el (or (first ($ (str "#" id)))
               (first ($ (str "<style id='" id "'></style>"))))]
    (aset el "innerHTML" s)
    (.appendChild (first ($ "head")) el)))

 

(def  app-state (atom 
{:splash {:idx 0}
 :context []
 :touch {:velocity [0 0]}
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



(component main [data owner opts]
  (render-state [_ state]
    (html
      (<div#main
        (onDragEnter 
          (fn [e] (aset (.-dataTransfer e) "dropEffect" "move") ))
        (<div#splash 
          (onClick (fn [e] (om/transact! data [:splash :idx] inc)))
          (into-array (map-indexed 
            #(<img (src %2)
              (style {:opacity 
                (if (<= (- 5 %1) (:idx (:splash data)))
                  0.0 1.0)}))
            splash-moons))
          (<h1 "Cabinet Selenica")
          (om/build searchbox data {}))
        (om/build book-scroller data {})))))


 
(om/root main app-state
  {:target (. js/document (getElementById "app"))})


(defn on-js-reload []

)

 