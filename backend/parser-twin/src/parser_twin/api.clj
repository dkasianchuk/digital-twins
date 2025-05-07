(ns parser-twin.api
  (:require [reitit.ring :as ring]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.multipart-params :refer [wrap-multipart-params]]
            [parser-twin.parser :as parser]
            [clojure.stacktrace :as stacktrace]))

(defn- handle-internal-error [handler]
  (fn [req]
    (try
      (handler req)
      (catch Throwable t
        (println "Unexpected error happened.")
        (stacktrace/print-cause-trace t 10)
        (flush)
        {:status 500 :body "Internal server error"}))))

(defn- add-predefined-headers [handler]
  (fn [req]
    (update
     (handler req)
     :headers
     #(assoc %
             "Access-Control-Allow-Origin" "http://localhost:5173"
             "Access-Control-Allow-Headers" "Content-Type"))))

(def router
  (->
   ["/v1" {:middleware [handle-internal-error add-predefined-headers]}
    ["/ping" {:get (fn [_] {:status 200 :body "Hello World!"})}]
    ["/generate-parser"
     {:post parser/handle-generate-parser}]
    ["/parse" {:get parser/handle-parse-req}]]
   (ring/router)
   (ring/ring-handler)
   (wrap-multipart-params)))

(defonce servers (atom {}))

(defn stop-server [port]
  (when-let [server (@servers port)]
    (.stop server)
    (swap! servers dissoc port)
    true))

(defn start-server [port]
  (let [server (jetty/run-jetty router {:port port :join? false})]
    (swap! servers assoc port server)
    true))

(defn restart-server [port]
  (stop-server port)
  (start-server port))

(defn -main [port]
  (start-server port))
