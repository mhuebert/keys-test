(ns keys-test.core
  (:require [re-view.core :as v :refer [defview]]
            [re-db.d :as d]
            [re-view-routing.core :as r]
            ["combokeys" :as CombokeysInit]
            ["keymaster" :as Keymaster]
            ["keypress.js" :as KP :refer [keypress]]
            [clojure.string :as string]
            [goog.object :as gobj]))

(defonce Combokeys (CombokeysInit. js/document.documentElement))

(defonce Keypress (new (.-Listener keypress)))
(r/listen #(d/transact! [(assoc % :db/id ::router)]))

;;;;;;;;;;;;;;;;;;;;;
;; Notes
;;
;; PREVENT DEFAULT: return `false` from a handler to prevent default, `true` to allow event to propagate
;;
;; ORDER: By default, Keypress we require that the user pressed the keys down in the same order that they are listed.
;;
;; Mousetrap
;; - Not maintained, using the `ComboKeys` fork
;; - LIMITATION: cannot bind Command-Shift-)
;;
;; Keypress
;; - Not actively maintained
;; - Keypress prevents default unless the handler returns `true`. Mousetrap does not prevent default, unless handler returns `false`.
;; - Keypress allows `shift-(`, Mousetrap only allows `(`, to capture (.
;; - BUG: Keypress cannot unregister meta bindings.
;;
;; Keymaster
;; - Last update 2 years ago
;; - LIMITATION: Keymaster does register bindings for single modifiers, eg. 'command'
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
(def space \u00A0)

(def decode-uri js/decodeURIComponent)
(defn encode-uri [s]
  (-> s
      (js/encodeURIComponent)
      (string/replace "(" "%28")
      (string/replace ")" "%29")))

(defn keymap [library]
  (case library :Combokeys {"M1"       "mod"
                            "M2"       "alt"
                            :separator "+"}
                :Keypress {"M1"       "meta"
                           "M2"       "alt"
                           :separator " "}
                :Keymaster {"M1"       (if mac? "command" "ctrl")
                            "M2"       "alt"
                            :separator "+"}
                :display {"M1"       (if mac?
                                       "Command"
                                       "Control")
                          "M2"       (if mac?
                                       "Option" "Alt")
                          "shift"    "Shift"
                          :separator "-"}
                :querystring {:separator " "}))

(defn sort-binding [binding]
  (->> binding
       (sort-by (fn [char]
                  [(if (modifiers-set char) 0 1) char]))
       (vec)))

(defn binding-string [library keys]
  (let [{:keys [separator] :as lib-keymap} (keymap library)]
    (->> (sort-binding keys)
         (remap-vec lib-keymap)
         (string/join separator))))

(defn bind [library binding action]
  (let [binding-string (binding-string library binding)]
    (prn (str "Binding: <" binding-string ">, Library: " (name library)))
    (case library
      :Combokeys
      (do
        (.bind Combokeys binding-string action)
        #(.unbind Combokeys binding-string))
      :Keymaster
      (do (Keymaster binding-string action)
          #(.unbind Keymaster binding-string))

      :Keypress
      (do
        (.simple_combo Keypress binding-string action)
        #(.unregister_combo Keypress binding-string)))))

(defview combo-test
  {:listen                  (fn [{:keys [view/props library binding view/prev-props view/state]}]
                              (let [{:keys [unbind]} @state]
                                (when unbind (unbind))
                                (swap! state assoc :unbind
                                       (bind library binding
                                             #(do
                                                (prn (str "Action: <" binding ">, Library: " (name library)))
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
    [:.pa3.f4.flex.flex-auto.bg-lightest-blue.mv3
     [:.flex-auto
      (if (seq binding)
        (list
          [:.gray.mv2.dark-blue "Please press: "]
          [:div.flex.items-stretch.bg-blue.bw3.b--light-blue.mv3
           [:.white.pa2.b.dib (if (seq binding) (binding-string :display binding) space)]
           (if captured? [:.ph2.bg-lightest-blue.dark-blue.flex.items-center.flex-auto "Captured"]
                         [:.ph2.bg-light-yellow.flex.items-center.flex-auto "Waiting..."])])
        [:.gray.pv2 "Please select a binding."])
      ]]))



(defn query-vals []
  (let [{:keys [keystring library]
         :or   {keystring ""}} (d/get ::router :query)]
    {:binding (disj (set (string/split (decode-uri keystring)
                                       #"\s")) "")
     :library (some-> library (keyword))}))

(defn show-char [selected the-char pmap state]
  (let [active? (selected the-char)]
    [:.pa1.dib.monospace
     {:on-mouse-down #(do (r/swap-query! assoc :keystring (encode-uri (binding-string :querystring ((if active? disj conj) (:binding (query-vals)) the-char))))
                          (.preventDefault %))
      :class         (if active? "b white bg-dark-gray" " hover-bg-darken pointer")}
     (get pmap the-char the-char)]))

(defview keys-test
  [{:keys [view/state]}]
  (let [{binding :binding
         library :library} (query-vals)
        display-pmap (keymap :display)]
    [:.monospace.pa3.mw6.center
     [:.flex-none
      (when-not library [:.b.ma3 "Please select a library: "])
      [:.f6.bg-darken-lightly
       (for [library-option [:Keypress :Combokeys :Keymaster]
             :let [active? (= library-option library)]]
         [:.pa2.ma2.f6.dib
          {:on-click #(r/swap-query! assoc :library (name library-option))
           :class    (if active? "b white bg-dark-gray" " hover-bg-darken pointer")}
          (name library-option)])]]
     (when library
       (list
         (combo-test {:library library
                      :binding binding})
         [:.f6.mv3
          [:.dib.pa2.blue.underline.pointer.hover-bg-darken {:on-click #(r/swap-query! dissoc :keystring)} "Clear"]
          [:.dib.pa2.f7.gray.i
           (name library) " string: \"" (binding-string library binding) \"
           ]]
         [:.bg-darken-lightly.f6.flex-auto.mw6.pv2
          (for [char-set all-chars]
            [:.mv2.ph3 (for [ch char-set]
                         (show-char binding ch display-pmap state))])])
       )]))

(defn ^:export render []
  (v/render-to-dom (keys-test) "app"))