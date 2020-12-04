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
;;; Sometimes cells also need to track the *dependencies* of other cells, to
;;; avoid dependency cycles, and to react to those being broken.
;;;
;;; Other than that, cell components only `deref` their own state, and the
;;; top-level spreadsheet component doesn't `deref` anything, so the effect of
;;; mutations is contained.
;;;
;;; Formulas are compiled into cljs functions (along with a description of their
;;; dependencies, if any) when the user edits them. We do that via a
;;; reagent.ratom/reaction called `source-reaction`. Dependency cycles are
;;; checked for and prevented at this time.
;;;
;;; Each cell has a `formula-reaction` reagent.ratom/reaction that watches for
;;; changes in the *values* of cells it depends on. The determination of which
;;; cells to watch for this purpose is made dynamically by watching
;;; `source-reaction`. This prevents cells from reacting to changes in cells
;;; they no longer depend on. `formula-reaction` recalculates the cell's value
;;; and pushes it to the value cursor.
;;;
;;; Since the `value-cursor`s track `formula-reaction` we could get rid of them
;;; and have cells watch the `formula-reaction` of their dependencies directly,
;;; but then we couldn't have a single atom holding the whole app state, as
;;; desired.
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


(defn find-path [[head :as path] target succ-fn]
  (when head
       (if (= head target)
         path
         (first (keep #(find-path (cons % path) target succ-fn)
                      (succ-fn head))))))


(defn find-dep-cycle [k watches deps]
  (first (keep #(find-path (cons % nil) k (comp deref (fn [x]
                                                        (prn "si" x (deps x))
                                                        (deps x))))
               watches)))


(defn run-in-peek-mode [f & args]
  (binding [ratom/*ratom-context* nil]
    (apply f args)))


(defn cell [k source-cursors value-cursors deps]
  (r/with-let [source-cursor (get source-cursors k)
               value-cursor (get value-cursors k)
               edit-text (r/atom nil)
               source-reaction (ratom/run!
                                (let [{:keys [watches] :as formula}
                                      (compile-src (or @source-cursor ""))]
                                  (let [cycle
                                        (run-in-peek-mode ; avoid tracking deps unless necessary
                                         find-dep-cycle k watches deps)]
                                    (if cycle
                                      ;; track dependency changes in the cycle,
                                      ;; so I can clear the error if the cycle
                                      ;; is broken by editing some other cell
                                      (do
                                        (dorun (map (comp deref deps) cycle))
                                        {:error "ERROR: Formula would introduce cycles"})
                                      (do (reset! (deps k) watches)
                                          formula)))))
               formula-reaction (ratom/run!
                                 (->> (let [{:keys [error] :as formula} @source-reaction]
                                        (or error
                                            (compute-formula formula value-cursors)))
                                      (reset! value-cursor)))]
    [:td {:title k     ; tooltip to double-check you're editing the right cell
          :on-double-click #(reset! edit-text (or @source-cursor ""))}
     (if (some? @edit-text)
       [cell-editor edit-text source-cursor]
       (str @value-cursor))]
    (finally
      (ratom/dispose! formula-reaction)
      (ratom/dispose! source-reaction))))

(defn cell-map [f]
  (into {}
        (for [row rows
              col cols
              :let [k (cell-key col row)]]
          [k (f k)])))


(defn spreadsheet []
  (let [state (r/atom {:cell-sources {}
                       :cell-values {}})
        source-cursors (cell-map #(r/cursor state [:cell-sources %]))
        value-cursors (cell-map #(r/cursor state [:cell-values %]))
        ;; for cycle detection; this is ultimately derived from `(:cell-sources state)`
        deps (cell-map #(r/atom #{}))]
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
