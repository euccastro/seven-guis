;;;
;;; Notes on state management
;;; =========================
;;;
;;; We track all authoritative app state in the `state` atom in the top-level
;;; `spreadsheet` component. This is not necessary for the requirements
;;; of the 7guis challenge, but it is done for illustration purposes. If we were
;;; doing Undo or load/save functionality, this is what we'd manage with those.
;;;
;;; What *is* necessary is to track the calculated cell *values* across cells
;;; so value changes can be propagated reactively. As an obscure corner case
;;; (see `dep-check-reaction` below for the details), sometimes cells need to
;;; track the *dependencies* of other cells, in order to clear any errors caused
;;; by previously detected dependency cycles.
;;;
;;; Other than that, cell components only `deref` their own state, and the
;;; top-level spreadsheet component doesn't `deref` anything, so the effect of
;;; mutations is contained.
;;;
;;; User-entered text is processed in the following pipeline:
;;; (Note: +> marks additional input from other cells)
;;;
;;; text in edit box (edit-text)
;;;
;;; -> working source (`source-cursors`, part of authoritative app state)
;;;    +> watched `deps` from other cells (if we have detected a cycle)
;;;
;;; -> compiled formula with checked dependencies (`dep-check-reaction`)
;;;    +> computed value of dependency cells (watched selectively from
;;;      `source-cursors`)
;;;
;;; -> evaluation (`eval-reaction`)
;;;
;;; -> computed value (`value-cursors`; part of authoratitative app state)
;;;
;;; At each stage, errors from previous ones are passed through, bypassing
;;; normal processing.
;;;
;;; Formulas are compiled into cljs functions (along with a description of their
;;; dependencies, if any) when the user edits them. We do that via a
;;; reagent.ratom/reaction called `source-reaction`. Dependency cycles are
;;; checked for and prevented at this time.
;;;
;;; Since the `value-cursor`s just track `eval-reaction`s without further
;;; processing, we *could* get rid of them and have cells watch the
;;; `eval-reaction` of their dependencies directly, but then we couldn't have a
;;; single atom holding the whole app state as a plain value, as desired.
;;;

(ns seven-guis.cells.components
  (:require [reagent.core :as r]
            [reagent.ratom :as ratom]
            [seven-guis.util :as util]
            [seven-guis.cells.formula :refer [cell-key compile-src]]))


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


(defn find-path [from to succ-fn]
  (if (= from to)
    (cons from nil)
    ;; Although this is depth-first search, we don't need to check for
    ;; cycles because by construction we don't have any (preventing
    ;; dependency cycles is the very purpose of this function in the
    ;; context of this namespace). Adding an assert would bump the big-O
    ;; cost of this function for no real benefit. If we had a cycle the
    ;; user would be screwed already, and it's not clear how we could help
    ;; them now. So I chose to "optimize for a correct program" here.
    (when-let [path (first (keep #(find-path % to succ-fn)
                                 (succ-fn from)))]
        (cons from path))))


(defn find-dep-cycle [k watches deps]
  (first (keep #(find-path % k (comp deref deps))
               watches)))


(defn run-in-peek-mode [f & args]
  (binding [ratom/*ratom-context* nil]
    (apply f args)))


(defn cell [k source-cursors value-cursors deps]
  (r/with-let [;; Holds transient text we're editing in this cell but haven't
               ;; commited to evaluation yet. Also signals editing mode.
               edit-text (r/atom nil)
               ;; Holds the results of the last complete edit of this cell.
               source-cursor (get source-cursors k)
               ;; Holds the value that this cell outputs
               value-cursor (get value-cursors k)
               ;; Checks for, and maintains, errors due to formula dependency
               ;; cycles.
               dep-check-reaction
               (ratom/run!
                (let [{:keys [watches] :as formula}
                      (compile-src (or @source-cursor ""))
                      cycle (run-in-peek-mode ; avoid tracking deps unless necessary
                             find-dep-cycle k watches deps)]
                    (if cycle
                      (do
                        ;; track dependency changes in the cycle, so I can clear
                        ;; the error if the cycle is broken by editing some
                        ;; other cell
                        (dorun (map (comp deref deps)
                                    (butlast  ; last is this cell
                                     cycle)))
                        {:error "ERROR: Formula would introduce cycles"})
                      (do (reset! (deps k) watches)
                          formula))))
               ;; Evaluates the compiled formula, producing output value.
               eval-reaction
               (ratom/run!
                (->> (let [{:keys [error] :as formula} @dep-check-reaction]
                       (or error
                           (compute-formula formula value-cursors)))
                     (reset! value-cursor)))]
    [:td {:title k     ; tooltip to double-check you're editing the right cell
          :on-double-click #(reset! edit-text (or @source-cursor ""))}
     (if (some? @edit-text)
       [cell-editor edit-text source-cursor]
       (str @value-cursor))]
    (finally
      (ratom/dispose! eval-reaction)
      (ratom/dispose! dep-check-reaction))))


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
