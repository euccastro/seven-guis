(ns seven-guis.circle-drawer
  (:require [clojure.edn :as edn]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [seven-guis.util :as util]))


(def initial-circle-radius 20)
(def max-circle-radius 100)


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


(defn close-transients
  "Close popup menu and resizing dialog, committing resize if necessary."
  [{:keys [tmp-selected-circle-radius selected-circle] :as old-state}]
  (cond-> old-state
    tmp-selected-circle-radius (add-to-history
                                (partial
                                 replace
                                 {selected-circle
                                  (assoc selected-circle :r tmp-selected-circle-radius)}))
    true (dissoc :menu-pos :dialog? :tmp-selected-circle-radius :selected-circle)))


(defn circle-drawer []
  (let [state (r/atom {:selected-circle nil
                       :tmp-selected-circle-radius nil
                       :menu-pos nil
                       :dialog? false
                       :history-index -1
                       :history []})]
    (fn []
      (let [{:keys [selected-circle
                    tmp-selected-circle-radius
                    menu-pos
                    dialog?
                    history-index
                    history]} @state
            any-transients? (or menu-pos dialog?)
            close-any-transients #(when any-transients?
                                    (.preventDefault %)
                                    (swap! state close-transients))
            circles (get history history-index)]
        [:div#root
         {:on-click close-any-transients
          :on-context-menu close-any-transients}
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
           "Redo"]]
         [:div ; get flex layout, let svg have position: relative so I can get bounds
          [:svg
           {:width 600
            :height 400
            :on-mouse-leave
            #(when-not any-transients? (swap! state dissoc :selected-circle))
            :on-mouse-move
            (fn [e]
              (when-not any-transients?  ; keep selection stable in these cases
                (let [[mx my] (mouse-event-coords e)
                      circle-sqdist-to-mouse #(squared-dist mx my (:cx %) (:cy %))]
                  (swap! state assoc :selected-circle
                         (->> circles
                              (filter #(< (circle-sqdist-to-mouse %)
                                          (Math/pow (:r %) 2)))
                              (apply min-key circle-sqdist-to-mouse))))))
            :on-click
            (fn [e]
              (when-not any-transients?
                (.stopPropagation e)
                (let [[mx my] (mouse-event-coords e)
                      new-circle {:id (str (random-uuid))
                                  :cx mx :cy my :r initial-circle-radius
                                  :stroke "black" :stroke-width 1
                                  :fill "none"}]
                  (swap! state
                         #(-> %
                              (dissoc :menu-pos)
                              (add-to-history conj new-circle)
                              (assoc :selected-circle new-circle))))))
            :on-context-menu
            (fn [e]
              (.preventDefault e)
              (if any-transients?
                (swap! state close-transients)
                (swap! state assoc :menu-pos (when selected-circle [(.-pageX e) (.-pageY e)]))))}
           (for [{:keys [id] :as c} circles
                 :when (not= c selected-circle)]
             ^{:key id} [:circle c])
           ;; always paint this one on top
           (when selected-circle
             [:circle (-> selected-circle
                          (update :r #(or tmp-selected-circle-radius %))
                          (assoc :fill "lightgray"))])]
          (when-let [[x y] menu-pos]
            [:div#popup-menu
             {:style {:left x :top y}
              :on-click (fn [e]
                          (.stopPropagation e)
                          (swap! state
                                 #(-> %
                                      (dissoc :menu-pos)
                                      (assoc :dialog? true))))}
             "Adjust diameter..."])]
         (when dialog?
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
                     :value (or tmp-selected-circle-radius (:r selected-circle))
                     :on-change
                     (fn [e]
                       (swap! state assoc :tmp-selected-circle-radius
                              (edn/read-string (util/evt-value e))))}]])]))))

(comment
  (js->clj evt)
  (.. evt -target getBoundingClientRect -left)
  (.offset (.-target evt))
  (.-pageX evt)
  (.-clientX evt)
  (.-offsetX evt))

(rdom/render [circle-drawer]
             (.getElementById js/document "app"))
