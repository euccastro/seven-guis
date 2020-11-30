(ns seven-guis.temperature-converter
  (:require [clojure.edn :as edn]
            [reagent.core :as r]
            [reagent.dom :as rdom]))

(enable-console-print!)

(comment
  (number? (js/parseFloat "not really a number!!11"))  ;; => true
  ;; so we can't use this
  )


(defn read-edn-if-valid
  "Returns nil on invalid edn, instead of crashing"
  [s]
  (try
    (edn/read-string s)
    (catch ExceptionInfo e
      (if (= (-> e .-data :type) :reader-exception)
        nil
        (throw e)))))

(comment
  (read-edn-if-valid "1")
  (read-edn-if-valid ":a")
  (read-edn-if-valid ")")
  (try
    (edn/read-string ")")
    (catch ExceptionInfo e (-> e .-data :type)))
  )

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
        converter {[celsius fahrenheit] celsius->fahrenheit
                   [fahrenheit celsius] fahrenheit->celsius}
        handler (fn [this other]
                  (fn [ev]
                    (let [val (-> ev .-target .-value)
                          ?parsed (read-edn-if-valid val)]
                      (reset! this val)
                      (when (number? ?parsed)
                        (reset! other ((converter [this other]) ?parsed))))))]
    (fn []
      [:div
       [:input
        {:type :text
         :value @celsius
         :on-change (handler celsius fahrenheit) }]
       " Celsius = "
       [:input
        {:type :text
         :value @fahrenheit
         :on-change (handler fahrenheit celsius)}]
       " Fahrenheit"])))


(rdom/render [temperature-converter]
             (.getElementById js/document "app"))
