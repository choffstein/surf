(defproject surf "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
		 [net.dnolen.clj-cont "0.1.0"]
		 [compojure "0.6.2"]
		 [ring "0.3.7"]
		 [net.cgrand/moustache "1.0.0"]
		 [hiccup "0.3.4"]]
  :dev-dependencies [[swank-clojure "1.3.0-SNAPSHOT"]
		     [marginalia "0.5.0" :exclusions [hiccup]]]
  :main surf.core
  :aot [surf.core])
