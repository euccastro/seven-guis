(ns seven-guis.cells
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [seven-guis.util :as util]))


(def rows (range 100))


(def cols "ABCDEFGHIJKLMNOPQRSTUVWXYZ")


(defn cell [k editing? state]
  (let [contents (r/atom nil)]
    (fn [k editing? state]
      [:td {:title k
            :on-double-click #(swap! state assoc :editing-cell k)}
       (if editing?
         [:input {:type :text
                  :value @contents
                  :on-blur #(swap! state dissoc :editing-cell)
                  :on-change #(reset! contents (util/evt-value %))}]
         @contents)])))


(defn counter []
  (let [state (r/atom {:editing-cell nil})]
    (fn []
      (let [{:keys [editing-cell]} @state]
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
                  :let [k (str col row)]]
              ^{:key k}
              [cell k (= k editing-cell) state])])]))))


(rdom/render [counter]
             (.getElementById js/document "app"))
