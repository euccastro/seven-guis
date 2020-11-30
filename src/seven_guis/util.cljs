(ns seven-guis.util
  (:require [clojure.edn :as edn]))


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


(defn evt-value [e]
  (.. e -target -value))


(defn text-input-for
  [ratom]
  [:input {:type :text
           :value @ratom
           :on-change #(reset! ratom (evt-value %))}])



(comment
  (read-edn-if-valid "1")
  (read-edn-if-valid ":a")
  (read-edn-if-valid ")")
  (try
    (edn/read-string ")")
    (catch ExceptionInfo e (-> e .-data :type)))
  )
