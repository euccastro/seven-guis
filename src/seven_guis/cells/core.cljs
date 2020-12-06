(ns seven-guis.cells.core
  (:require [reagent.dom :as rdom]
            [seven-guis.cells.components :refer [spreadsheet]]))


(rdom/render [spreadsheet]
             (.getElementById js/document "app"))
