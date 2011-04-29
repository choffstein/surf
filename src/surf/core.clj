(ns surf.core
  (:use [compojure.core]
	[ring.adapter.jetty]
	[ring.util.response]
	[ring.middleware.params]
	[net.cgrand.moustache]
	[hiccup core page-helpers form-helpers]
	[delimc.core])
  (:require [compojure.route :as route]
	    [clojure.contrib.string :as string])
  (:import (java.util UUID)))

(defn- generate-uuid [] (.toString (UUID/randomUUID)))

;;;;;;;;;; SESSIONS
(def sessions (atom {}))

(defrecord session [id pages])
(defn new-session [page]
  (session. (generate-uuid) (atom {(:id page) page})))

(defn add-page-to-session [session page]
   (swap! (:pages session) assoc (:id page) page))

;;;;;;;;;; HTML
(defn html-doc
  "A basic HTML document construction"
  [title & body]
  (html
   (doctype :html4)
   [:html
    [:head
     [:title title]]
      [:body
       body]]))

;;;;;;;;; COMPONENT
(defprotocol Component
  "A component"
  (render [component session page] "Render the component into html form")
  (nom [component] "Return the component name")
  (clone [component] "Clones the current component")
  (children [component] "Child components")
  (update-children [component children] "Update child components")
  (id [component] "Return a unique id for the component"))

;;;;;;;;; ENVIRONMENT
(defrecord page [id root-component callbacks])

(defn tabula-rasa [component]
  (page. (generate-uuid) component (atom [])))

;; could probably replace true / false passing with continuations
(defn update-page-helper [component component-to-update fn callback-input]
  (if (= (id component) (id component-to-update))
    [true (apply fn (cons component-to-update callback-input))]
    (let
	[children (children component)
	 updates (map
		  #(update-page-helper % component-to-update fn callback-input) children)
	 did-update (reduce #(or %1 (first %2)) true updates)
	 updated-children (map second updates)]
      (if did-update
	[true (update-children component updated-children)]
	[false component]))))

(defn update-page-with [page component fn callback-input]
  (let [root-component (:root-component page)]
    (tabula-rasa (second (update-page-helper root-component component fn callback-input)))))

(defn render-page [session page]
  (let [root-component (:root-component page)]
    (html-doc (:nom root-component)
	      (render root-component session page))))

;;;;;;;; URL
(defn construct-url [session page & callback-id]
  (let [extension (if (not (nil? callback-id)) (str "&_a=" (first callback-id)) "")]
    (str (:nom (:root-component page)) "?_s=" (:id session) "&_p=" (:id page) extension)))
  
;;;;;;;; CALLBACKS
(defn construct-callback [component callback session page & input-order]
  (let [cc (atom nil)
	existing-callbacks (:callbacks page)
	callback-id (count @existing-callbacks)]
    ;; create a continuation that executes the call-back,
    ;; creates a new page, and redirects to i
    (do
      (reset
       (let [callback-input-hash (shift k
					(reset! cc k))
	     
	     callback-input (if (not (nil? input-order))
			      (map #(get callback-input-hash %) input-order)
			      [])
	     _ (println input-order)
	     _ (println callback-input-hash)
	     _ (println callback-input)
	     
	     new-page (update-page-with page component callback callback-input)]
	 (do (add-page-to-session session new-page)
	     (redirect (construct-url session new-page)))))

      (swap! existing-callbacks conj cc)
      (construct-url session page callback-id))))

;;;;;;;;;;; COUNTER COMPONENT
(defn increase-counter [counter-component]
  (do (println "Increase Counter")
  (assoc counter-component :state (inc (:state counter-component)))))

(defn decrease-counter [counter-component]
  (do (println "Decrease Counter")
  (assoc counter-component :state (dec (:state counter-component)))))

(defrecord counter-component [id state]
  Component
  (render [self session page]
	  [:div
	   [:a {:href (construct-callback self decrease-counter session page)} "--"]
	   [:b (:state self)]
	   [:a {:href (construct-callback self increase-counter session page)} "++"]])
  (nom [self] "counter")
  (clone [self] (counter-component. (generate-uuid) (:state self)))
  (children [self] [])
  (update-children [self children] self)
  (id [self] (:id self)))

(defn new-counter-component [init]
  (counter-component. (generate-uuid) init))

(defn counter-root []
  (let [counter (new-counter-component 0)
	page (tabula-rasa counter)]
    page))

;;;;;;;;;;;;; MULTIPLE COUNTERS COMPONENT
(defrecord multiple-counters-component [id counters]
  Component
  (render [self session page]
	  (map #(render % session page) counters))
  (nom [self] "page")
  (clone [self] (multiple-counters-component. (generate-uuid) (map clone counters)))
  (children [self] (:counters self))
  (update-children [self children] (multiple-counters-component. (generate-uuid) children))
  (id [self] (:id self)))

(defn new-multiple-counters-component [counters]
  (multiple-counters-component. (generate-uuid) counters))

(defn multiple-counters-root []
  (let [root-component (new-multiple-counters-component
			[(new-counter-component 0) (new-counter-component 0)])
	page (tabula-rasa root-component)]
    page))

;;;;;;;;;;;;; PARAMS
(defn- split-params [params]
  (let [key-vals (string/split #"&" params)]
    (reduce #(let [[key value] (string/split #"=" %2)]
	       (assoc %1 key value)) {} key-vals)))

(defn- extract-params [req]
  (let [query-string (or (:query-string req) "")
	handle (apply str (rest (:uri req)))
	split-params (split-params query-string)]
    (assoc split-params "component" handle)))

;;;;;;;;;;;;; COMPONENT REGISTRY
(def component-root-page
     {"counter" counter-root
      "index" multiple-counters-root})

(defn- handle-request [req]
  (let [params (extract-params req)

	session-id (get params "_s") 
	session (get @sessions session-id)
	
	page-id (get params "_p")
	page (if (not (nil? session))
	       (get @(:pages session) page-id)
	       nil)

	action-id-str (get params "_a")
	action-id (if (not (nil? action-id-str)) (Integer/parseInt action-id-str) nil)
	component-id (get params "component")]
    ;; if we don't have a session yet,
    ;; construct one and register a new environment
    ;; and go to the component root
    (if (or (nil? session) (nil? page))
      (let [root-page ((get component-root-page component-id))
	    session (new-session root-page)]
	(do (swap! sessions assoc (:id session) session)
	    (response (render-page session root-page))))
      ;; we already have a session -- is this an action?
      (if (nil? action-id)
	;; no, so just render the current environment
	(response (render-page session page))
	;; otherwise, update the environment based on the action,
	;; add it to the session, and redirect to the
	;; new url
	(let [callback (nth @(:callbacks page) action-id)
	      form-params (:form-params req)]
	  (@callback form-params))))))
     
(def main-routes
     (app
      [_] (wrap-params (fn [req] (handle-request req)))))
      
(defn run []
  (run-jetty #'main-routes {:port 9000 :join? false})) 