(ns seven-guis.cells
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [seven-guis.util :as util]))


(def rows (range 100))


(def cols "ABCDEFGHIJKLMNOPQRSTUVWXYZ")


(defn cell-editor [edit-text source]
  [:input
   {:type :text
    :auto-focus true
    :on-focus #(.. % -currentTarget select)
    :on-key-down #(case (.-keyCode %)
                    ;; commit and exit on ENTER
                    (13) (.. % -target blur)
                    ;; cancel on ESC
                    (27) (reset! edit-text nil)
                    true)
    :value @edit-text
    :on-blur #(let [t @edit-text]
                (when (string? t)
                  (reset! source t)
                  (reset! edit-text nil)))
    :on-change #(reset! edit-text (util/evt-value %))}])


(defn cell [k cursor]
  (r/with-let [source (r/atom "")
               edit-text (r/atom nil)
               _ (add-watch source k
                            (fn [_ _ _ new-val]
                              (reset! cursor (str "eval " new-val))))]
    [:td {:title k  ; tooltip to double-check you're editing the right cell
          :on-double-click #(reset! edit-text @source)}
     (if (some? @edit-text)
       [cell-editor edit-text source]
       @cursor)]
    (finally (remove-watch source k))))


(defn cells []
  (let [state (r/atom {:cell-contents {}})
        cursors (into {}
                      (for [row rows
                            col cols]
                        [[row col] (r/cursor state [:cell-contents [row col]])]))]
    (fn []
      [:table
       [:tr
        [:th]  ; blank corner
        (for [col cols] ^{:key col} [:th col])]
       (for [row rows]
         ^{:key row}
         [:tr
          [:th row]
          (for [col cols
                :let [k (str row col)]]
            ^{:key (str col row)}
            [cell k (cursors [row col])])])])))


(rdom/render [cells]
             (.getElementById js/document "app"))
