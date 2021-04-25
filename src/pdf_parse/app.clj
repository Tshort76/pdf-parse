(ns pdf-parse.app
  (:require [clojure.java.io :as io]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [environ.core :refer [env]]
            [org.httpkit.server :as server]
            [pdf-parse.core :as core]
            [pdf-parse.annotations :as ann]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.multipart-params :as mp]
            [ring.middleware.json :refer [wrap-json-response]]
            [ring.util.http-response :as r])
  (:import (java.io File)))

(def output-file "annotations.pdf")

(defn as-format [fmt]
  (or
   ((set (keys core/page-parser)) (keyword fmt))
   :components))

(defn int-nil [x & [default]]
  (try
    (Integer/parseInt x)
    (catch NumberFormatException e (print (.getMessage e)))
    (finally (or default 0))))

(def content-type {:json "application/json"
                   :pdf "application/pdf"})

(defn ok-typed [body type]
  (-> body r/ok (update :headers assoc "Content-Type" (content-type type))))

(defn parse-pdf [file format & [{:keys [start-page end-page keep-file]}]]
  (let [^File tf (:tempfile file)
        data (core/transform (io/input-stream tf)
                             {:page-bounds [(int-nil start-page) (int-nil end-page 5)]
                              :format format})]
    (when-not keep-file (.delete tf))
    data))

(defn annotate [data {:keys [tempfile]} format]
  (let [pdf-url (str (.toURI tempfile))
        opts (cond-> {:pdf-url pdf-url :out-file output-file}
               (= format :components) (assoc :table-columns? true :superscripts? true))]
    (dorun (ann/annotate opts data))
    (.delete tempfile)
    (File. output-file)))


(defroutes app-routes
  (mp/wrap-multipart-params
   (POST "/v1/transform" [file 
                          format :<< as-format 
                          start-page
                          end-page]
    (ok-typed (parse-pdf file format {:start-page start-page
                                      :end-page end-page})
              :json)))
  (mp/wrap-multipart-params
   (POST "/v1/annotate" [file
                         format :<< as-format
                         start-page
                         end-page]
     (-> file
         (parse-pdf format {:start-page start-page
                            :end-page end-page
                            :keep-file true})
         (annotate file format)
         (ok-typed :pdf))))
  (route/not-found "Error, page not found!"))


;; clj -X pdf-parse.app/-main
(defn -main
  [& args]
  (let [port (Integer/parseInt (env :port "3000"))]
    (server/run-server 
     (-> app-routes
         (wrap-defaults (assoc-in site-defaults [:security :anti-forgery] false))
         wrap-json-response)
     {:port port})
    (println (str "Running webserver at http:/127.0.0.1:" port "/"))))