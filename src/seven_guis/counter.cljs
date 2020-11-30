(ns seven-guis.counter
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]))


(defn counter []
  (let [cnt (r/atom 0)]
    (fn []
      [:div @cnt [:button
                  {:on-click #(swap! cnt inc)}
                  "Increase"]])))


(rdom/render [counter]
             (.getElementById js/document "app"))
