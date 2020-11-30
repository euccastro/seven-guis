(ns seven-guis.temperature-converter
  (:require [seven-guis.util :refer [read-edn-if-valid]]
            [reagent.core :as r]
            [reagent.dom :as rdom]))

(enable-console-print!)


(defn celsius->fahrenheit [c]
  (+ (/ (* c 9) 5) 32))

(defn fahrenheit->celsius [f]
  (/ (* (- f 32) 5) 9))

(comment
  (celsius->fahrenheit -1)
  (celsius->fahrenheit 0)
  (celsius->fahrenheit 20)
  (fahrenheit->celsius 0)
  (fahrenheit->celsius 68))


(defn temperature-converter []
  (let [celsius (r/atom "")
        fahrenheit (r/atom "")
        convert-from {celsius  celsius->fahrenheit
                      fahrenheit fahrenheit->celsius}
        other (fn [x] (if (identical? x celsius)
                        fahrenheit
                        celsius))
        handler (fn [this]
                  (fn [ev]
                    (let [val (-> ev .-target .-value)
                          ?parsed (read-edn-if-valid val)]
                      (reset! this val)
                      (when (number? ?parsed)
                        (reset! (other this)
                                ((convert-from this)
                                 ?parsed))))))]
    (fn []
      (let [input (fn [which]
                    [:input {:type :text
                             :value @which
                             :on-change (handler which)}])]
        [:<> (input celsius) " Celsius = " (input fahrenheit) " Fahrenheit"]))))


(rdom/render [temperature-converter]
             (.getElementById js/document "app"))
