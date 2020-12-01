(ns seven-guis.circle-drawer
  (:require [clojure.edn :as edn]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [seven-guis.util :as util]))


(def initial-circle-radius 20)
(def max-circle-radius 200)


(defn mouse-event-coords
  "Like [(.-clientX e) (.-clientY e)] if that worked within a flex layout"
  [e]
  (let [canvas (let [t (.-target e)]
                 (if (= (.-tagName t) "svg")
                   t
                   ;; handle event bubbled up from a circle
                   (.-parentElement t)))
        bounds (.getBoundingClientRect canvas)]
    [(int (- (.-pageX e) (.-left bounds)))
     (int (- (.-pageY e) (.-top bounds)))]))


(defn squared-dist [ax ay bx by]
  (let [dx (- ax bx)
        dy (- ay by)]
    (+ (* dx dx) (* dy dy))))


(defn add-to-history
  "Add a circle state at current point in history, truncating if necessary.

  The state to add is calculated by applying `mutator` to the currently viewed
  state plus any other `args`, and coercing the result to a vector."
  [{:keys [history-index history] :as state} mutator & args]
  (let [truncated-history (subvec history 0 (inc history-index))
        new-history (conj truncated-history (vec (apply mutator (last truncated-history) args)))]
    (-> state
        (assoc :history new-history)
        (assoc :history-index (dec (count new-history))))))


(defn current-circles [{:keys [history history-index]}]
  (get history history-index))


(defn selected-circle [{[mx my] :mouse-pos :as state}]
  (let [circle-sqdist-to-mouse #(squared-dist mx my (:cx %) (:cy %))]
    (->> (current-circles state)
         (filter #(< (circle-sqdist-to-mouse %)
                     (Math/pow (:r %) 2)))
         (apply min-key circle-sqdist-to-mouse))))


(defn update-selected-circle [state]
  (assoc state :selected-circle (selected-circle state)))


(defn close-transients
  "Close popup menu and resizing dialog, committing resize if necessary."
  [{:keys [tmp-selected-circle-radius selected-circle] :as old-state}]
  (cond-> old-state
    tmp-selected-circle-radius (add-to-history
                                (partial
                                 replace
                                 {selected-circle
                                  (assoc selected-circle :r tmp-selected-circle-radius)}))
    true (dissoc :menu-pos :dialog? :tmp-selected-circle-radius)
    ;; We've been pinning the selection while transients were active, so let's
    ;; refresh it without waiting for the user to move the mouse.
    true update-selected-circle))


(defn any-transients? [{:keys [menu-pos dialog?]}]
  (or menu-pos dialog?))


(def no-transients? (complement any-transients?))


(defn update-if [pred? f]
  (fn [state & args]
    (if (pred? state)
      (apply f state args)
      state)))


(defn buttons [state]
  (let [{:keys [history history-index]} @state]
    [:div#buttons
     [:button
      {:disabled (< history-index 0)
       :on-click (fn [_]
                   (swap! state
                          #(-> %
                               (dissoc :selected-circle)
                               (update :history-index dec))))}
      "Undo"]
     [:button
      {:disabled (= history-index (dec (count history)))
       :on-click #(swap! state update :history-index inc)}
      "Redo"]]))


(defn canvas [state]
  (let [{:keys [selected-circle tmp-selected-circle-radius]} @state]
    [:div ; get flex layout, let svg have `position: relative` so I can get bounds
     [:svg
      {:width 600
       :height 400
       :on-mouse-leave
       #(swap! state (update-if no-transients? dissoc) :selected-circle)
       :on-mouse-move
       (fn [e]
         (swap! state
                (fn [old]
                  (cond-> (assoc old :mouse-pos (mouse-event-coords e))
                    ;; keep selection stable while transients are around
                    (no-transients? old) update-selected-circle))))
       :on-click
       (fn [e]
         (when (no-transients? @state)
           (.stopPropagation e)
           (let [[mx my] (mouse-event-coords e)
                 new-circle {:id (str (random-uuid))
                             :cx mx :cy my :r initial-circle-radius
                             :stroke "black" :stroke-width 1
                             :fill "none"}]
             (swap! state
                    (update-if no-transients?
                               #(-> %
                                    (dissoc :menu-pos)
                                    (add-to-history conj new-circle)
                                    (assoc :selected-circle new-circle)))))))
       :on-context-menu
       (fn [e]
         (.preventDefault e)
         (.stopPropagation e)
         (swap! state
                (fn [old]
                  (if (any-transients? old)
                    (close-transients old)
                    (assoc old :menu-pos (when selected-circle [(.-pageX e) (.-pageY e)]))))))}
      ;; always paint this one behind, so we can see every outline
      (when selected-circle
        [:circle (-> selected-circle
                     (update :r #(or tmp-selected-circle-radius %))
                     (assoc :fill "lightgray"))])
      (for [{:keys [id] :as c} (current-circles @state)
            :when (not= c selected-circle)]
        ^{:key id} [:circle c])]]))


(defn menu [state x y]
  [:div#popup-menu
   {:style {:left x :top y}
    :on-click (fn [e]
                (.stopPropagation e)
                (swap! state
                       #(-> %
                            (dissoc :menu-pos)
                            (assoc :dialog? true))))}
   "Adjust diameter..."])


(defn dialog [state]
  [:dialog {:open true
            :on-click #(.stopPropagation %)}
   (str "Adjust diameter of circle at ("
        (:cx selected-circle)
        ", "
        (:cy selected-circle)
        ")")
   [:input {:type :range
            :min "1"
            :max max-circle-radius
            :value (or (:tmp-selected-circle-radius @state) (:r selected-circle))
            :on-change
            (fn [e]
              (swap! state assoc :tmp-selected-circle-radius
                     (edn/read-string (util/evt-value e))))}]])


(defn circle-drawer []
  (r/with-let [state (r/atom {:selected-circle nil
                              :tmp-selected-circle-radius nil
                              :menu-pos nil
                              :dialog? false
                              :history-index -1
                              :history []})
               global-listener (fn [e]
                                 (when (any-transients? @state)
                                   (.preventDefault e)
                                   (swap! state close-transients)))
               _ (.addEventListener js/document "click" global-listener)
               _ (.addEventListener js/document "contextmenu" global-listener)]
    (let [{:keys [menu-pos dialog?]} @state]
      [:div#root
       [buttons state]
       [canvas state]
       (when-let [[x y] menu-pos]
         [menu state x y])
       (when dialog?
         [dialog state])])
    (finally
      (.removeEventListener js/document "click" global-listener)
      (.removeEventListener js/document "contextmenu" global-listener))))


(rdom/render [circle-drawer]
             (.getElementById js/document "app"))
