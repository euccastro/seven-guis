(ns seven-guis.circle-drawer
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]))


(def initial-circle-radius 20)


(defn mouse-event-coords
  "Like [(.-clientX e) (.-clientY e)] if that worked within a flex layout"
  [e]
  (let [bounds (.. e -target getBoundingClientRect)]
    [(- (.-pageX e) (.-left bounds))
     (- (.-pageY e) (.-top bounds))]))


(defn squared-dist [ax ay bx by]
  (let [dx (- ax bx)
        dy (- ay by)]
    (+ (* dx dx) (* dy dy))))


(defn circle-drawer []
  (let [state (r/atom {:selected-circle nil
                       :circles []})]
    (fn []
      (let [{:keys [circles selected-circle]} @state]
        [:div#root
         [:div#buttons
          [:button "Undo"]
          [:button "Redo"]]
         [:div ; get flex layout, let svg have position: relative so I can get bounds
          [:svg
           {:width 600
            :height 400
            :on-mouse-move
            (fn [e]
              (let [[mx my] (mouse-event-coords e)]
                (swap! state assoc :selected-circle
                       (apply min-key #(squared-dist mx my (:cx %) (:cy %)) circles))))
            :on-click
            (fn [e]
              (let [[mx my] (mouse-event-coords e)
                    new-circle {:id (str (random-uuid))
                                :cx mx :cy my :r initial-circle-radius
                                :stroke "black" :stroke-width 1
                                :fill "white"
                                :on-mouse-move #(.stopPropagation %)}]
                (swap! state
                       #(-> %
                            (update :circles conj new-circle)
                            (assoc :selected-circle new-circle)))))}
           (for [{:keys [id] :as c} circles]
             ^{:key id} [:circle (cond-> c
                                   (identical? c selected-circle)
                                   (assoc :fill "lightgray"))])]]]))))

(comment
  (js->clj evt)
  (.. evt -target getBoundingClientRect -left)
  (.offset (.-target evt))
  (.-pageX evt)
  (.-clientX evt)
  (.-offsetX evt))

(rdom/render [circle-drawer]
             (.getElementById js/document "app"))
