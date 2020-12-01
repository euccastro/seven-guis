(ns seven-guis.crud
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [seven-guis.util :as util]
            [clojure.string :as str]))


(defn crud []
  (let [state (r/atom {:filter-prefix ""
                       :selected-item nil
                       :name ""
                       :surname ""
                       :db [{:id (random-uuid) :name "Hans" :surname "Emil"}
                            {:id (random-uuid) :name "Max" :surname "Mustermann"}
                            {:id (random-uuid) :name "Roman" :surname "Tisch"}]})]
    (fn []
      (let [{:keys [filter-prefix selected-item name surname db]} @state]

        [:div#root

         [:div#filter
          [:label {:for "prefix"} "Filter prefix:"]
          [:input {:type :text
                   :value filter-prefix
                   :on-change #(swap! state assoc :filter-prefix (util/evt-value %))}]]

         ;; I tried using a <select> but I had trouble selecting/deleting the first item
         [:div#person-list
          [:ul
           (or (seq (for [{:keys [id name surname] :as entry} db
                          :when (str/starts-with? surname filter-prefix)
                          :let [select #(swap! state assoc :selected-item entry)]]
                      ^{:key id} [:li (cond-> {:tab-index 0
                                               :on-click select
                                               :on-key-press select}
                                        (= entry selected-item)
                                        (assoc :class "selected"))
                                  (str surname ", " name)]))
               [:li {:disabled true}
                (if (seq db)
                  "<No items matching prefix>"
                  "<No items in db>")])]]

         [:div#name-inputs
          [:div
           [:label {:for "name"} "Name:"]
           [:input#name {:type :text
                         :value name
                         :on-change #(swap! state assoc :name (util/evt-value %))}]]
          [:div
           [:label {:for "surname"} "Surname:"]
           [:input#surname {:type :text
                            :value surname
                            :on-change #(swap! state assoc :surname (util/evt-value %))}]]]

         [:div#buttons
          [:button
           {:disabled (some empty? [name surname])
            :on-click (fn [_]
                        (swap! state update :db conj
                               {:id (random-uuid) :name name :surname surname}))}
           "Create"]
          [:button
           {:disabled (some empty? [name surname selected-item])
            :on-click (fn [_]
                        (let [new-item (assoc selected-item :name name :surname surname)]
                          (swap! state
                                 (fn [old]
                                   (-> old
                                       (assoc :selected-item new-item)
                                       (update :db (partial replace {selected-item new-item})))))))}
           "Update"]
          [:button
           {:disabled (empty? selected-item)
            :on-click (fn [_]
                        (swap! state
                               (fn [old]
                                 (-> old
                                     (update :db #(vec (remove #{selected-item} %)))
                                     (dissoc :selected-item)))))}
           "Delete"]]]))))


(rdom/render [crud]
             (.getElementById js/document "app"))
