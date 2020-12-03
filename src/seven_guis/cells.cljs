;;;
;;; Notes on state management
;;; =========================
;;;
;;; We track all authoritative app state in the `state` atom in the top-level
;;; `spreadsheet` component. This is not necessary for the requirements
;;; of the 7guis challenge, but it is done for illustration purposes. If we were
;;; doing Undo or load/save functionality, this is what we'd manage with those.
;;;
;;; What *is* necessary is to track some ratom(s) for cell *values* in a
;;; centralized way, so cells can watch changes in the value of other cells.
;;;
;;; Each cell only `deref`s its own state (plus any dependencies in its formula;
;;; see below), and the top-level spreadsheet component doesn't `deref`
;;; anything, so the effect of mutations is contained.
;;;
;;; Formulas are compiled into cljs functions (along with a description of their
;;; dependencies, if any) only when the user edits them. We do that via a
;;; reagent.ratom/reaction called `source-reaction`. Dependency cycles are
;;; checked for and prevented at this time.
;;;
;;; Each cell has a `formula-reaction` reagent.ratom/reaction that watches for
;;; changes in the *values* of cells it depends on. The determination of which
;;; cells to watch for this purpose is made dynamically by watching
;;; `source-reaction`. This prevents cells from reacting to changes in cells
;;; they no longer depend on. `formula-reaction` recalculates the cell's value
;;; and pipes it into the value cursor via a `clojure.core/watch`. We could get
;;; rid of this watch by having everyone interested in this cell's value `deref`
;;; its `formula-reaction` directly, but that would make it harder to get hold
;;; of the wholprogram state for Undo etc.
;;;

(ns seven-guis.cells
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [reagent.ratom :as ratom]
            [seven-guis.util :as util]
            [seven-guis.cells-formula :refer [cell-key compile-src]]))


(def rows (range 100))
(def cols (util/char-range "A" "Z"))


(defn cell-editor [edit-text source-cursor]
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
                  (reset! source-cursor t)
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


(defn cell [k source-cursors value-cursors deps]
  (r/with-let [value-cursor (get value-cursors k)
               source-cursor (get source-cursors k)
               edit-text (r/atom nil)
               source-reaction (ratom/run!
                                (let [{:keys [watches] :as formula}
                                      (compile-src (or @source-cursor ""))]
                                  (if (would-introduce-cycles? k watches @deps)
                                    {:error "ERROR: Formula would introduce cycles"}
                                    formula)))
               formula-reaction (ratom/run!
                                 (let [{:keys [error] :as formula} @source-reaction]
                                   (if error
                                     error
                                     (compute-formula formula value-cursors ))))
               _ (add-watch source-reaction k
                            (fn [_ _ _ {:keys [error watches]}]
                              (when-not error
                                (swap! deps assoc k watches))))
               _ (add-watch formula-reaction k
                            (fn [_ _ _ new-val]
                              (reset! value-cursor new-val)))]
    [:td {:title k     ; tooltip to double-check you're editing the right cell
          :on-double-click #(reset! edit-text (or @source-cursor ""))}
     (if (some? @edit-text)
       [cell-editor edit-text source-cursor]
       (str @value-cursor))]
    (finally
      (ratom/dispose! formula-reaction)
      (ratom/dispose! source-reaction))))


(defn cursors [state top-level-k]
  (into {}
        (for [row rows
              col cols
              :let [k (cell-key col row)]]
          [k (r/cursor state [top-level-k k])])))


(defn spreadsheet []
  (let [state (r/atom {:cell-sources {}
                       :cell-values {}})
        source-cursors (cursors state :cell-sources)
        value-cursors (cursors state :cell-values)
        ;; for cycle detection; this is ultimately derived from `(:cell-sources state)`
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
                 :let [k (cell-key col row)]]
             ^{:key k}
             [cell k source-cursors value-cursors deps])])]])))


(rdom/render [spreadsheet]
             (.getElementById js/document "app"))
