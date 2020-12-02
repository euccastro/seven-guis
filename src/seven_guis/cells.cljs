(ns seven-guis.cells
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [reagent.ratom :as ratom]
            [seven-guis.util :as util]
            [seven-guis.cells-formula :as formula]))


(def rows (range 100))
(def cols (util/char-range "A" "Z"))


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


(defn compute-formula [{:keys [watches f]} cursors]
  (f (zipmap watches (map (comp deref cursors) watches))))


(defn would-introduce-cycles? [k watches deps]
  (loop [visited #{}
         [x & frontier-rest] watches]
    (cond
      (nil? x) false
      (= x k) true
      :else (recur (conj visited x)
                   (concat frontier-rest (remove visited (get deps x)))))))


(comment
  ;; In production code I'd add property-based tests for this.
  (would-introduce-cycles? "A1" '() {})
  (would-introduce-cycles? "A1" '("A1") {})
  (would-introduce-cycles? "A1" '("B1") {})
  (would-introduce-cycles? "A1" '("B1" "C1") {"B1" '("C1")})
  (would-introduce-cycles? "A1" '("B1") {"C1" '("A1")})
  (would-introduce-cycles? "A1" '("B1" "C1") {"B1" '("C1")
                                              "C1" '("A1")})
  )


(defn cell [k cursors deps]
  (r/with-let [my-cursor (get cursors k)
               source (r/atom "")
               edit-text (r/atom nil)
               source-reaction (ratom/run!
                                (let [{:keys [watches] :as formula} (formula/parse @source)]
                                  (if (would-introduce-cycles? k watches @deps)
                                    {:error "ERROR: Formula would introduce cycles"}
                                    formula)))
               formula-reaction (ratom/run!
                                 (let [{:keys [error] :as formula} @source-reaction]
                                   (if error
                                     error
                                     (compute-formula formula cursors))))
               _ (add-watch source-reaction k
                            (fn [_ _ _ {:keys [error watches]}]
                              (when-not error
                                (swap! deps assoc k watches))))
               _ (add-watch formula-reaction k
                            (fn [_ _ _ new-val]
                              (reset! my-cursor new-val)))]
    [:td {:title k     ; tooltip to double-check you're editing the right cell
          :on-double-click #(reset! edit-text @source)}
     (if (some? @edit-text)
       [cell-editor edit-text source]
       (str @my-cursor))]
    (finally
      (ratom/dispose! formula-reaction)
      (ratom/dispose! source-reaction))))


(defn cells []
  (let [state (r/atom {:cell-contents {}})
        cursors (into {}
                      (for [row rows
                            col cols]
                        [(formula/cell-key col row) (r/cursor state [:cell-contents [row col]])]))
        ;; for cycle detection
        deps (atom {})]
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
                 :let [k (formula/cell-key col row)]]
             ^{:key k}
             [cell k cursors deps])])]])))


(rdom/render [cells]
             (.getElementById js/document "app"))
