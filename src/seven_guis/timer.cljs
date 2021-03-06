(ns seven-guis.timer
  (:require [seven-guis.util :as util]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [clojure.edn :as edn]))

(enable-console-print!)

;;; All times in seconds

(def max-end 60)
(def tick-step 0.1)


(defn seconds->ms [t]
  (int (* t 1000)))


(declare adjust-timer)


(defn tick [state-atom]
  (fn []
    (swap! state-atom update :elapsed + tick-step)
    (adjust-timer state-atom)))


(defn set-timer [state-atom]
  (swap! state-atom assoc :timer
         (js/setInterval (tick state-atom) (seconds->ms tick-step))))


(defn clear-timer [state-atom]
  (swap! state-atom
         (fn [{:keys [timer] :as old}]
           (when timer (js/clearInterval timer))
           (dissoc old :timer))))


(defn adjust-timer [state-atom]
  (let [{:keys [elapsed end timer]} @state-atom]
    (cond
      (and (nil? timer) (< elapsed end))
      (set-timer state-atom)
      (and (some? timer) (>= elapsed end))
      (clear-timer state-atom))))


(defn timer-component []
  (r/with-let [state (doto (r/atom {:end (/ max-end 2)
                                    :elapsed 0})
                       adjust-timer
                       (add-watch ::watch #(adjust-timer %2)))]
    (let [{:keys [elapsed end]} @state]
      [:<>
       [:div
        [:label {:for "progress"} "Elapsed Time: "]
        [:meter#progress
         {:value elapsed :min "0" :max end}
         (str elapsed " out of " end)]]
       [:div (str (.toFixed elapsed 1) "s")]
       [:div
        [:label {:for "duration"} "Duration: "]
        [:input {:type :range
                 :min "1"
                 :max max-end
                 :value end
                 :on-change
                 (fn [e]
                   (swap! state assoc :end
                          (edn/read-string (util/evt-value e))))}]]
       [:div
        [:button {:on-click (fn [_]
                              (swap! state assoc :elapsed 0))}
         "Reset"]]])
    (finally (remove-watch state ::watch)
             (clear-timer state))))


(rdom/render [timer-component]
             (.getElementById js/document "app"))
