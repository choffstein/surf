(ns surf.core
  (:use [compojure.core]
	[ring.adapter.jetty]
	[ring.util.response]
	[ring.middleware.params]
	[net.cgrand.moustache]
	[hiccup core page-helpers])
  (:require [compojure.route :as route]
	    [clojure.contrib.string :as string]
	    [net.dnolen.clj-cont :as clj-cont])
  (:import (java.util UUID)))

(defn- generate-uuid [] (.toString (UUID/randomUUID)))

(def sessions (ref {}))

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

(defn construct-callback [component fn]
  (dosync
   (let [session-id (generate-uuid)]
     (do
       (alter sessions assoc session-id {:fn fn
					 :component component})
       session-id))))

(defrecord counter-component [state])

(defn increase-counter [counter-component]
  (assoc counter-component :state (inc (:state counter-component))))

(defn decrease-counter [counter-component]
  (assoc counter-component :state (dec (:state counter-component))))

(defn render-counter [counter-component]
  (html-doc "Counter"
	    [:a {:href (construct-callback counter-component decrease-counter)} "--"]
	    [:b (:state counter-component)]
	    [:a {:href (construct-callback counter-component increase-counter)} "++"]))

(defn- handle-session [req]
  (dosync
   (let [session-key (last (string/split #"/" (:uri req)))
	 session (get @sessions session-key)]
     (if (nil? session)
       (let [new-component (counter-component. 0)]
	 (render-counter new-component))
       (render-counter ((:fn session) (:component session)))))))
     
(def main-routes
     (app
      [_] (fn [req] {:status 200
		     :body (handle-session req)})))
      
(defn run []
  (run-jetty #'main-routes {:port 8080 :join? false})) 