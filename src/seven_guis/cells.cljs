(ns seven-guis.cells
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [reagent.ratom :as ratom]
            [seven-guis.util :as util]
            [clojure.string :as str]))


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


(defn mock-parse-formula [src]
  (let [watches (map second
                     (re-seq #"\{\{(\w+)\}\}" src))]
    {:watches watches
     :f (fn [watches]
          (if (seq watches)
            (pr-str watches)
            src))}))


(defn compute-formula [{:keys [watches f]} cursors]
  (f (zipmap watches (map (comp deref cursors) watches))))


(defn cell-key [col row]
  (str col row))


(defn cell [k cursors]
  (r/with-let [my-cursor (get cursors k)
               source (r/atom "")
               edit-text (r/atom nil)
               formula-reaction (ratom/run! (compute-formula (mock-parse-formula @source) cursors))
               _ (add-watch formula-reaction k
                            (fn [_ _ _ new-val]
                              (println "New val!" new-val)
                              (reset! my-cursor new-val)))]
    [:td {:title k     ; tooltip to double-check you're editing the right cell
          :on-double-click #(reset! edit-text @source)}
     (if (some? @edit-text)
       [cell-editor edit-text source]
       (str @my-cursor))]
    (finally (ratom/dispose! formula-reaction))))


(defn cells []
  (let [state (r/atom {:cell-contents {}})
        cursors (into {}
                      (for [row rows
                            col cols]
                        [(cell-key col row) (r/cursor state [:cell-contents [row col]])]))]
    (fn []
      [:table
       [:thead
        [:tr
         [:th]                          ; blank corner
         (for [col cols] ^{:key col} [:th col])]]
       [:tbody
        (for [row rows]
          ^{:key row}
          [:tr
           [:th row]
           (for [col cols
                 :let [k (cell-key col row)]]
             ^{:key k}
             [cell k cursors])])]])))


(rdom/render [cells]
             (.getElementById js/document "app"))
