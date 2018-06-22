(ns spotify
  (:require [clojure.java.io  :as io]
            [clojure.string   :as str]
            [clj-spotify.core :as s]
            [clj-spotify.util :as sutil]))

(def ^:const client-id     (System/getenv "SPOTIFY_CLIENT_ID"))
(def ^:const client-secret (System/getenv "SPOTIFY_CLIENT_SECRET"))

(def ^:dynamic *access-token* nil)

(defn refresh-access-token!
  []
  (->> (sutil/get-access-token client-id client-secret)
       constantly
       (alter-var-root #'*access-token*)))

(refresh-access-token!)

(defn api-results
  [api-fn {:keys [response-path] :as m} & args]
  (let [response (apply api-fn
                        (-> m (dissoc :response-path) (assoc :limit 50))
                        (conj args *access-token*))
        _ (when-let [error (:error response)]
            (throw (ex-info "Spotify API call failed." error)))
        {:keys [offset limit total items]} (get-in response
                                                   (or response-path []))]
    (if (> total (+ offset limit))
      (conj items
            (apply api-results
                   api-fn
                   (assoc m :offset (+ offset limit))
                   args))
      items)))

(defn normalize-name
  [s]
  (-> s str/lower-case (str/replace #"[^A-Za-z0-9]" "")))

(defn search-artists
  [artist-name]
  (->> (api-results s/search {:q artist-name
                              :type "artist"
                              :response-path [:artists]})
       (filter #(when-let [name (:name %)]
                  (= (normalize-name artist-name) (normalize-name name))))))

(defn albums
  [artist]
  (api-results s/get-an-artists-albums artist))

(defn find-album
  [artist-name album-name]
  (->> (search-artists artist-name)
       (map (comp
              (fn [albums]
                (->> albums
                     (filter #(when-let [name (:name %)]
                                (str/includes? (normalize-name name)
                                               (normalize-name album-name))))
                     first))
              albums))
       (remove empty?)
       first))

(comment
  (def nas-albums (->> (slurp "/home/dave/nas-albums")
                       (#(str/split % #"\n"))
                       (mapv #(str/split % #"\t"))))

  (def workloads (partition-all 324 nas-albums))

  (def todo    (atom (nth workloads 0)))
  (def todo    (atom (nth workloads 1)))
  (def todo    (atom (nth workloads 2)))
  (def todo    (atom (nth workloads 3)))
  (def results (java.util.concurrent.LinkedBlockingQueue.))
  (def logged  (atom 0))

  (def log-file "/home/dave/nas-albums.log")
  (defn log [msg] (spit (io/file log-file) (str msg \newline) :append true))

  (defn take! [n]
    (let [return-value (atom nil)]
      (dosync
        (swap! todo #(let [[batch the-rest] (split-at n %)]
                       (reset! return-value batch)
                       the-rest))
        @return-value)))

  (def working (atom true))

  ;; restart (do this, then re-eval queue-filler and result-collector)
  (reset! working true)
  ;; stop
  (do
    (reset! working false)
    (Thread/sleep 2500)
    (future-cancel queue-filler)
    (future-cancel result-collector))

  (def queue-filler
    (future
      (log "starting queue-filler")
      (while (and @working (< @logged (count nas-albums)))
        (log "taking 5...")
        (doseq [[artist album] (take! 5)]
          (future
            (try
              (.put results [artist album (if (find-album artist album)
                                            "on-spotify"
                                            "not-on-spotify")])
              (catch clojure.lang.ExceptionInfo e
                (let [{:keys [status message]} (ex-data e)]
                  (cond
                    (= 429 status)
                    (do
                      (log "rate limit exceeded. re-queuing...")
                      (swap! todo conj [artist album]))

                    (= "The access token expired" message)
                    (do
                      (log "access token expired. refreshing and re-queuing...")
                      (refresh-access-token!)
                      (swap! todo conj [artist album]))

                    :else
                    (.put results [artist album e]))))
              (catch Throwable e
                (.put results [artist album e])))))
        (log "sleeping...")
        (Thread/sleep 60000))
      (log "stopping queue-filler")))

  (def result-collector
    (future
      (log "starting result-collector")
      (while (and @working (< @logged (count nas-albums)))
        (let [[artist album result] (.take results)]
          (log [artist album result])
          (cond
            (#{"on-spotify" "not-on-spotify"} result)
            (spit (io/file (str "/home/dave/nas-albums." result))
                  (str (str/join " - " [artist album]) \newline)
                  :append true)

            (instance? Throwable result)
            (log (str/join \newline
                           (cons (.getMessage result)
                                 (.getStackTrace result))))

            :else
            (log (str "Unexpected result: " result)))
          (swap! logged inc)))
      (log "stopping result-collector"))))

