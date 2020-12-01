(ns seven-guis.cells
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [seven-guis.util :as util]))


(def rows (range 100))


(def cols "ABCDEFGHIJKLMNOPQRSTUVWXYZ")


(defn cell [k contents state]
  [:td {:title k
        :on-double-click #(swap! state assoc :editing-cell k)}
   (if (= k (:editing-cell @state))
     [:input {:type :text
              :value contents
              :on-blur #(swap! state dissoc :editing-cell)
              :on-change #(swap! state assoc-in [:cell-contents k] (util/evt-value %))}]
     contents)])


(defn counter []
  (let [state (r/atom {:cell-contents {}
                       :editing-cell nil})]
    (fn []
      [:table
       [:tr
        [:th]
        (for [col cols]
          ^{:key col}
          [:th col])]
       (for [row rows]
         ^{:key row}
         [:tr
          [:th row]
          (for [col cols
                :let [k (str col row)
                      cell-contents (get-in @state [:cell-contents k])]]
            ^{:key k}
            [cell k cell-contents state])])])))


(rdom/render [counter]
             (.getElementById js/document "app"))
