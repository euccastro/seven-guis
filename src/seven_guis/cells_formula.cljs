(ns seven-guis.cells-formula)


(defn parse-formula [src]
  (let [watches (map second
                     (re-seq #"\{\{(\w+)\}\}" src))]
    {:watches watches
     :f (fn [watch-m]
          (if (seq watch-m)
            (pr-str watch-m)
            src))}))
