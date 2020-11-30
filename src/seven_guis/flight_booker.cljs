(ns seven-guis.flight-booker
  (:require [seven-guis.util :as util]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [clojure.string :as str]))

(enable-console-print!)


(def default-date-str
  (let [d (js/Date.)]
    (str/join "." [(.getDate d) (+ (.getMonth d) 1) (+ 1900 (.getYear d))])))


(defn parse-date [s]
  ;; use [YYYY MM DD] internally for ease of comparison
  ;; in real life I'd use a date picker and a date parsing library; I don't
  ;; suppose I'm expected to handle every possible date format or reject every
  ;; invalid date, so I just do basic smoke tests.
  (let [[year month day :as fields]
        (mapv util/read-edn-if-valid (reverse (str/split s #"\.")))]
    (when (and (= (count fields) 3)
               (every? integer? fields)
               (<= 1 day 31)
               (<= 1 month 12)
               (< 1900 year))
      fields)))


(comment
  (util/read-edn-if-valid nil)  ; => nil
  (parse-date "27.03.2014")
  (< (parse-date "27.03.2020") (parse-date "28.03.2020")))


(defn disable-text-input [text-input]
  (assoc-in text-input [1 :disabled] true))

(defn mark-text-input-invalid [text-input]
  (assoc-in text-input [1 :style :background-color] :red))


(defn flight-booker []
  (let [one-way? (r/atom true)
        departure-date-str (r/atom default-date-str)
        return-date-str (r/atom default-date-str)]
    (fn []
      (let [[departure-date return-date :as dates]
            (map parse-date [@departure-date-str @return-date-str])
            can-book? (and (every? identity dates)
                           (or @one-way?
                               (<= departure-date return-date)))
            book-message
            (str "You have booked a "
                 (if @one-way? "one way" "return")
                 " flight departing on " @departure-date-str
                 (when-not @one-way?
                   (str " and returning on " @return-date-str))
                 ".")]
        [:div
         [:div
          [:select {:on-change #(reset! one-way?
                                        (= (util/evt-value %) "one-way"))}
           [:option {:value "one-way"} "one-way flight"]
           [:option {:value "return"} "return flight"]]]
         [:div
          (cond-> (util/text-input-for departure-date-str)
            (nil? departure-date) mark-text-input-invalid)]
         [:div
          (cond-> (util/text-input-for return-date-str)
            @one-way? disable-text-input
            (nil? return-date) mark-text-input-invalid)]
         [:div
          [:button {:disabled (not can-book?)
                    :on-click #(js/alert book-message)}
           "Book"]]]))))


(rdom/render [flight-booker]
             (.getElementById js/document "app"))
