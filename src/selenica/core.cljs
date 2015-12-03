(ns ^:figwheel-always selenica.core
  (:require-macros
    [selenica.macros :refer [mapf ? ..! each log]]
    [heh.core :refer [html component]])
  (:require
    [clojure.string :as string]
    [cljs.pprint]
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

(defn vec-dissoc [col & idxs]
  (vec (for [i (range (count col)) :when (not ((set idxs) i))] (get col i))))

(defn valid-idx [col n] (min (max 0 n) (dec (count col))))

(defn vec-move [col idx offset]
  (let [v (get col idx)
        res (vec-dissoc col idx)
        target (valid-idx col (+ idx offset))]
    (vec (concat (subvec res 0 target) [v] (subvec res target (count res))))))



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
    #_ (set! (.-responseType req) "arraybuffer")
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
 :inventory #_ DATA/INVENTORY (edn-> (get-local "inventory")) 
 :editing {:id nil}
  }))

(defn root-ref [& more]
  (om/ref-cursor (get-in (om/root-cursor app-state) more)))
 

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
    (let [scheme (:schema opts)]
    (html
      (<div.vector
        (if-not (vector? (:form scheme))
          (<button.new-field "+" (onClick (fn [_] (om/transact! data #(conj % ""))))))
          (into-array (map-indexed
            (fn [i v]
              (<div
                (when-not (vector? (:form scheme))
                  (<span 
                    (<button "-" (onClick (fn [_] (om/transact! data #(vec-dissoc % i)))))
                    (<button "^" (onClick (fn [_] (om/transact! data #(vec-move % i -1)))))
                    (<button "v" (onClick (fn [_] (om/transact! data #(vec-move % i 1)))))))
                (<input 
                  (value (str v))
                  (onChange #())
                  (onBlur (fn [e] (om/transact! data [i] #(str (.. e -target -value))))))))
            data))
        (<hr))))))
 
(component listing-editor [data owner opts]
  (render-state [_ state]
    (html
      (<div.editing
        (<hr)
          (<span 
            (<button "save"
              (onClick #(do 
                (om/transact! (root-ref :editing) (fn [o] (dissoc o :id)))
                (put-local "inventory" (->edn @(root-ref :inventory))) )))
            (<button "discard"
              (onClick #(om/update-state! owner :editing not))))
          (<hr)
          (into-array (map 
            (fn [[k v]]
              (let [scheme (get schema k)
                    key-str (.-name k)
                    small-field? (#{:short :number :integer} scheme)]
                (<label key-str
                  (class (str key-str (if small-field? " small"))) 
                  (cond (:vector scheme)
                        (om/build vec-editor (get data k) {:opts {:schema scheme}})
                        small-field?
                        (<input 
                          (value (str v))
                          (onChange (fn [e] ))
                          (onBlur (fn [e] (om/transact! data [k] #(str (.. e -target -value))))))
                        (#{:long} scheme)
                        (<textarea (value (str v))
                          (onChange (fn [e] ))
                          (onBlur (fn [e] (om/transact! data [k] #(str (.. e -target -value))))))))))
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
      (<div.listing
        (<div.meta 
          (<span.date (:date data))
          (<span.form (str (:domain data) " - " (:language data)))
          (<span.dimensions 
            (prn-str (:dimensions data)))
          (when-not (= (:id (root-ref :editing)) (str (:id data)))
            (<button "edit"
              (onClick 
                (fn [_]
                  (if-let [entry (get (root-ref :inventory) (str (:id data)))]
                    (om/transact! (root-ref :editing) [:id] (fn [_] (str (:id data))))
                    (prn "no entry"))))))
          (<span.condition (:condition data) (class (:condition data) ))
          (<span.id (:id data)))
        (<div.layout
        (<span.title (:title data))

        (<div.details 
          
          (<span.author (:author data))
          (into-array (map
            #(let [
              english (get-in DATA/english [(:id data) (keyword %)])
              french (get data (keyword %))]
              (when (or english french)
                (<div (<label (str % ": ")) 
                  (<span (class (if english "english" "french")) 
                    (or english french)))))
            ['description 'publisher 'observations 'notes 'materials 'provenance 'links])))
        
          (if-let [s (get (:images data) (or (:selected-img state) 0))]
            (<img.showcase (src (str "img/" s))))

        (<div.images 
            (into-array 
              (map-indexed 
                (fn [i s] 
                  (<img (src (str "img/" s))
                    (class (if (= (or (:selected-img state) 0) i) "selected" ""))
                    (onClick #(om/set-state! owner :selected-img i))))
                (:images data)))))))))
   

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
        (into-array (map #(om/build listing (last %) {:opts {:K (first %)}}) 
          (sort-by #((:date book-sorts) (last %))
            (filter 
              (comp (every-pred  
                (:valid-date book-filters)
                #_(:french book-filters)
                #_(:map book-filters)
                #_(complement (:french book-filters))
                ) last)
            (:inventory data)))))))))

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
        (om/build (get views (:screen (:view data))) data {})
        (if (:id (:editing data)) 
          (<div.two-panel (om/build listing (get (:inventory data) (:id (:editing data))) {})
                (om/build listing-editor (get (:inventory data) (:id (:editing data))) {})))))))



#_(om/root main app-state
  {:target (. js/document (getElementById "app"))})



(inject-css "ipfs"
  "body{
    background: rgb(70, 70, 70);
  }
    span.ipfs{
    padding: .2em .2em .2em .2em;
    margin:0.2em;
    text-align:left;
    height: 1em;
    line-height: 1em;
    overflow: hidden;
    display: inline-block;
    border-radius: 3px;
    font-family: 'Consolas', courier, mono-spaced;
    font-size:0.8em;
  }")




(def IPFS (atom {:hashes [
  "qms2hjwx8qejwm4nmwu7ze6ndam2sfums3x6idwz5myzbn"
  "qmv8ndh7ageh9b24zngaextmuhj7aiuw3scc8hkczvjkww" 
  "qmuvjja4s4cgyqyppozttssquvgcv2n2v8mae3gnkrxmol" 
  "qmrgjmlhlddhvxuieveuuwkeci4ygx8z7ujunikzpfzjuk" 
  "qmrolalcquyo5vu5v8bvqmgjcpzow16wukq3s3vrll2tdk" 
  "qmwk51jygpchgwr3srdnmhyerheqd22qw3vvyamb3emhuw"
  "QmSrCRJmzE4zE1nAfWPbzVfanKQNBhp7ZWmMnEdbiLvYNh"
  "QmQwAP9vFjbCtKvD8RkJdCvPHqLQjZfW7Mqbbqx18zd8j7"
  "QmTkzDwWqPbnAh5YiV5VwcTLnGdwSNsNTn2aDxdXBFca7D"]}))

(def ipfs-pattern #"[a-zA-Z0-9]{46}")

(component ipfs [data owner opts]
  (render-state [_ state]
    (let [[a b] (mapv (comp #(.toString % 16) #(.abs js/Math %) hash) 
                      (re-seq #"[a-zA-Z0-9]{23}" (:value data)))]
    (html
      (<span.ipfs
      (style {:background-color (str "#" (apply str (take 6 a)))
        :color "black"
        :border-left "1.5em solid" 
        :border-color (str "#" (apply str (take 6 b)))
              }) 
      (str b))))))

(component ipfs-app [data owner opts]
  (render-state [_ state]
    (html
      (<div (into-array
        (map #(om/build ipfs {:value %} {})
          (:hashes data)))))))

(om/root ipfs-app IPFS
  {:target (. js/document (getElementById "app"))})


