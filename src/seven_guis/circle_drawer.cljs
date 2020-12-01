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


(defn circle-drawer []
  (let [state (r/atom {:selected-circle nil
                       :tmp-selected-circle-radius nil
                       :menu-pos nil
                       :dialog? false
                       :circles []})]
    (fn []
      (let [{:keys [selected-circle tmp-selected-circle-radius menu-pos dialog? circles]} @state]
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
              (when-not (or menu-pos dialog?)  ; keep selection stable in these cases
                (let [[mx my] (mouse-event-coords e)
                      circle-sqdist-to-mouse #(squared-dist mx my (:cx %) (:cy %))]
                  (swap! state assoc :selected-circle
                         (->> circles
                              (filter #(< (circle-sqdist-to-mouse %)
                                          (Math/pow (:r %) 2)))
                              (apply min-key circle-sqdist-to-mouse))))))
            :on-click
            (fn [e]
              (if (or menu-pos dialog?)
                (swap! state dissoc :menu-pos :dialog?)
                (let [[mx my] (mouse-event-coords e)
                      new-circle {:id (str (random-uuid))
                                  :cx mx :cy my :r initial-circle-radius
                                  :stroke "black" :stroke-width 1
                                  :fill "none"}]
                  (swap! state
                         #(-> %
                              (dissoc :menu-pos)
                              (update :circles conj new-circle)
                              (assoc :selected-circle new-circle))))))
            :on-context-menu
            (fn [e]
              (.preventDefault e)
              (swap! state assoc :menu-pos (when selected-circle [(.-pageX e) (.-pageY e)])))}
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
                          (swap! state
                                 #(-> %
                                      (dissoc :menu-pos)
                                      (assoc :dialog? true))))}
             "Adjust diameter..."])]
         (when dialog?
           [:dialog {:open true}
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
