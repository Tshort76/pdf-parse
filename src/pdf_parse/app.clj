(ns pdf-parse.app
  (:require [org.httpkit.server :as server]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [environ.core :refer [env]]
            [ring.middleware.multipart-params :as mp]
            [pdf-parse.core :as core]
            [clojure.java.io :as io]
            [clojure.data.json :as json])
  (:import (java.io File)))


(defn simple-body-page [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    "Hello World"})

; request-example
(defn request-example [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body (str "Request Object: " req)})


(defn parse-pdf [file req]
  (let [^File tf (:tempfile file)
        resp (core/transform (io/input-stream tf) {:page-bounds [0 2] :format :components})]
    (.delete tf)
    {:status 200
     :headers {"Content-Type" "application/json"}
     :body resp})
  
  )


(defroutes app-routes
  (GET "/" [] simple-body-page)
  (GET "/request" [] request-example)
  (mp/wrap-multipart-params
   (POST "/transforms/components" {params :params} (parse-pdf (:file params) nil)))
  (route/not-found "Error, page not found!"))


(defn -main
  "This is our main entry point"
  [& args]
  (let [port (Integer/parseInt (env :port "3000"))]
    ; Run the server with Ring.defaults middleware
    (server/run-server (wrap-defaults #'app-routes (assoc-in site-defaults [:security :anti-forgery] false)) {:port port})
    ; Run the server without ring defaults
    ;(server/run-server #'app-routes {:port port})
    (println (str "Running webserver at http:/127.0.0.1:" port "/"))))