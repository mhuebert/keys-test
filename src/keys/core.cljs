(ns keys.core
  (:require [re-view.core :as v :refer [defview]]))

(defview key-test []
         [:div "Key Test"])

(defn render []
      (v/render-to-dom (key-test) "keys"))