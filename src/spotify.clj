(ns spotify
  (:require [clojure.string   :as str]
            [clj-spotify.core :as s]
            [clj-spotify.util :as sutil]))

(def ^:const client-id     (System/getenv "SPOTIFY_CLIENT_ID"))
(def ^:const client-secret (System/getenv "SPOTIFY_CLIENT_SECRET"))
(def ^:const access-token  (sutil/get-access-token client-id client-secret))

(defn api-results
  [api-fn {:keys [response-path] :as m} & args]
  (let [response (apply api-fn
                        (-> m (dissoc :response-path) (assoc :limit 50))
                        (conj args access-token))
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

