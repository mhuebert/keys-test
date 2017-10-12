(ns keys-test.core
  (:require [re-view.core :as v :refer [defview]]
            [re-db.d :as d]
            [re-view-routing.core :as r]
            ["mousetrap" :as ^js/Mousetrap Mousetrap]
            [clojure.string :as string]
            [goog.object :as gobj]))

(r/listen #(d/transact! [(assoc % :db/id ::router)]))
;;;;;;;;;;;;;;;;;;;;;
;; Notes
;;
;; PREVENT DEFAULT: return `false` from a handler to prevent default, `true` to allow event to propagate
;;
;; ORDER: By default, Keypress we require that the user pressed the keys down in the same order that they are listed.
;;
;;
;; Differences:
;; - Keypress prevents default unless the handler returns `true`. Mousetrap does not prevent default, unless handler returns `false`.
;; - Keypress allows `shift-(`, Mousetrap only allows `(`, to capture (.
;; - BUG: Keypress cannot unregister meta bindings.
;;
;; TODO
;; - show browser-agent for copy/paste


(def modifiers ["M1" "M2" "shift"])
(def modifiers-set (set modifiers))
(def all-chars [modifiers
                ["backspace" "tab" "enter" "return" "capslock" "esc" "escape" "space" "left" "up" "right" "down" "ins" "del" "plus"]
                (vec (seq "!@#$%^&*()_+-=[]\\{}|;':\",./<>?"))
                (vec (seq "abcdefghijklmnopqrstuvwxyz"))
                (vec (seq "1234567890"))])

(defn remap-vec [pmap coll]
  (mapv (fn [x] (get pmap x x)) coll))

(def mac? (re-find #"Mac|iPod|iPhone|iPad" (.-platform js/navigator)))


(defn pmap [library]
  (case library :Mousetrap {"M1" "mod"
                            "M2" "alt"}
                :display {"M1" (if mac?
                                 "Command"
                                 "Control")
                          "M2" (if mac?
                                 "Option" "Alt")
                          "shift" "Shift"}
                :querystring {}))

(defn binding-string [library keys]
  (let [separator (case library :Mousetrap "+"
                                :display "-"
                                :querystring " ")]
    (->> (remap-vec (pmap library) keys)
         (string/join separator))))

(defn bind [library binding action]
  (let [binding-string (binding-string library binding)]
    (case library
      :Mousetrap
      (do
        (.bind Mousetrap binding-string action)
        #(.unbind Mousetrap binding-string)))))

(defview combo-test
  {:listen                  (fn [{:keys [view/props library binding view/prev-props view/state]}]
                              (let [{:keys [unbind]} @state]
                                (when unbind (unbind))
                                (swap! state assoc :unbind
                                       (bind library binding
                                             #(do
                                                (d/transact! [[:db/update-attr ::state :captured (fnil conj #{})
                                                               [library binding]]])
                                                false)))))
   :view/did-mount          #(.listen %)
   :view/will-receive-props (fn [{:keys [view/props library binding view/prev-props view/state] :as this}]
                              (let [{:keys [unbind]} @state]
                                (when (not= props prev-props)
                                  (.listen this))))}
  [{:keys [view/state binding library]}]
  (let [captured? (contains? (d/get ::state :captured) [library binding])]
    [:.pa3.f4.flex.tc
     [:.ml4
      (if-not (seq binding)
        "Please select a key combination."
        (list
          [:span.b "Please press: "]
          [:div.inline-flex.items-stretch.bg-blue.bw3.b--light-blue
           [:.white.pa2.b.dib (binding-string :display binding)]
           (if captured? [:.ph2.bg-lightest-blue.dark-blue.flex.items-center "Captured"]
                         [:.ph2.bg-light-yellow.flex.items-center "Waiting..."])]))]]))

(defn sort-binding [binding]
  (->> binding
       (sort-by (fn [char]
                  [(if (modifiers-set char) 0 1) char]))
       (vec)))

(defn query-vals []
  (let [{:keys [keystring library]
         :or   {keystring ""
                library   "Mousetrap"}} (d/get ::router :query)]
    {:binding (disj (set (string/split (js/decodeURIComponent keystring)
                                       #"\s")) "")
     :library (keyword library)}))

(defn show-char [selected the-char pmap state]
  (let [active? (selected the-char)]
    [:.pa1.dib.monospace
     {:on-mouse-down #(do (r/swap-query! assoc :keystring (js/encodeURIComponent (binding-string :querystring ((if active? disj conj) (:binding (query-vals)) the-char))))
                          (.preventDefault %))
      :class         (if active? "b white bg-dark-gray" " hover-bg-darken pointer")}
     (get pmap the-char the-char)]))

(defview keys-test
  [{:keys [view/state]}]
  (let [{binding :binding
         library :library} (query-vals)
        display-pmap (pmap :display)]
    [:.monospace.pa3
     [:.flex.items-center.f6.bg-darken-lightly.pa3
      [:.b.mr2 "Library: "]
      (for [library-option [#_:Keypress :Mousetrap]
            :let [active? (= library-option library)]]
        [:.pa2.dib.f6
         {:on-click #(r/swap-query! assoc :library (name library-option))
          :class    (if active? "b white bg-dark-gray" " hover-bg-darken pointer")}
         (name library-option)])]

     (combo-test {:library library
                  :binding (sort-binding binding)})

     [:.bg-darken-lightly.f6
      [:.b.bg-darken.pointer.hover-bg-darken-more.ph3.pv2 {:on-click #(r/query-nav! {})} "Clear"]
      (for [char-set all-chars]
        [:.mv2.ph3 (for [ch char-set]
                     (show-char binding ch display-pmap state))])]]))

(defn ^:export render []
  (v/render-to-dom (keys-test) "app"))