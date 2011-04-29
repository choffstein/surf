(ns surf.core
  (:use [compojure.core]
	[ring.adapter.jetty]
	[ring.util.response]
	[ring.middleware.params]
	[net.cgrand.moustache]
	[hiccup core page-helpers]
	[delimc.core])
  (:require [compojure.route :as route]
	    [clojure.contrib.string :as string])
  (:import (java.util UUID)))

(defn- generate-uuid [] (.toString (UUID/randomUUID)))

;;;;;;;;;; SESSIONS
(def sessions (ref {}))

(defrecord session [id pages])
(defn new-session [page]
  (session. (generate-uuid) (ref {(:id page) page})))

(defn add-page-to-session [session page]
  (dosync
   (alter (:pages session) assoc (:id page) page)))

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
  (nom [component] "Return the component name"))

;;;;;;;;; ENVIRONMENT
(defrecord page [id root-component callbacks])

(defn tabula-rasa [component]
  (page. (generate-uuid) component (ref [])))

(defn render-page [session page]
  (let [root-component (:root-component page)]
    (render root-component session page)))

;;;;;;;; URL
(defn construct-url [component session page & callback-id]
  (let [extension (if (not (nil? callback-id)) (str "&_a=" (first callback-id)) "")]
    (str (:nom component) "?_s=" (:id session) "&_p=" (:id page) extension)))
  
;;;;;;;; CALLBACKS
(defn construct-callback [component callback session page]
  (dosync
   (let [cc (atom nil)
	 existing-callbacks (:callbacks page)
	 callback-id (count @existing-callbacks)]
     ;; create a continuation that executes the call-back,
     ;; creates a new page, and redirects to i
     (do
       (reset
	(let [garbage (shift k
			     (reset! cc k))
	      new-component (callback component)
	      new-page (tabula-rasa new-component)]
	      (do (add-page-to-session session new-page)
		  (redirect (construct-url new-component session new-page)))))

       (alter existing-callbacks conj cc)
       (construct-url component session page callback-id)))))

;;;;;;;;;;; COUNTER COMPONENT
(defn increase-counter [counter-component]
  (do (println "Increase Counter")
  (assoc counter-component :state (inc (:state counter-component)))))

(defn decrease-counter [counter-component]
  (do (println "Decrease Counter")
  (assoc counter-component :state (dec (:state counter-component)))))

(defrecord counter-component [state]
  Component
  (render [self session page]
	  (html-doc "Counter"
		    [:a {:href (construct-callback self decrease-counter session page)} "--"]
		    [:b (:state self)]
		    [:a {:href (construct-callback self increase-counter session page)} "++"]))
  (nom [self] "counter"))

(defn counter-root []
  (let [counter (counter-component. 0)
	page (tabula-rasa counter)]
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
     {"counter" counter-root})

(defn- handle-request [req]
  (dosync
   (let [params (extract-params req)
	 session-id (get params "_s") 
	 page-id (get params "_p")
	 action-id-str (get params "_a")
	 action-id (if (not (nil? action-id-str)) (Integer/parseInt action-id-str) nil)
	 component-id (get params "component")]
     ;; if we don't have a session yet,
     ;; construct one and register a new environment
     ;; and go to the component root
     (if (nil? session-id)
       (let [root-page ((get component-root-page component-id))
	     session (new-session root-page)]
	 (do (alter sessions assoc (:id session) session)
	     (response (render-page session root-page))))
       ;; we already have a session -- is this an action?
       (let [session (get @sessions session-id)
	     page (get @(:pages session) page-id)]
	 (if (nil? action-id)
	   ;; no, so just render the current environment
	   (response (render-page session page))
	   ;; otherwise, update the environment based on the action,
	   ;; add it to the session, and redirect to the
	   ;; new url
	   (let [callback (nth @(:callbacks page) action-id)
		 _ (println callback)]
	     (@callback nil)))))))) ;;nil is a hack here
     
(def main-routes
     (app
      [_] (fn [req] (handle-request req))))
      
(defn run []
  (run-jetty #'main-routes {:port 9000 :join? false})) 