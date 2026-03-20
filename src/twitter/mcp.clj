(ns twitter.mcp
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [twitter.api :as api]
            [twitter.format :as fmt])
  (:import [java.io BufferedReader InputStreamReader])
  (:gen-class))

(def ^:private tools
  [{:name "trending"
    :description "Get current trending topics on Twitter/X."
    :inputSchema {:type "object"
                  :properties {}}}
   {:name "search"
    :description "Search for tweets on Twitter/X."
    :inputSchema {:type "object"
                  :properties {:query {:type "string"
                                       :description "Search query"}
                               :n {:type "number"
                                   :description "Number of results (default 20)"}
                               :sort {:type "string"
                                      :description "Sort: Top or Latest (default Top)"}}
                  :required ["query"]}}
   {:name "timeline"
    :description "Get your home timeline on Twitter/X."
    :inputSchema {:type "object"
                  :properties {:n {:type "number"
                                   :description "Number of tweets (default 20)"}}}}
   {:name "read_thread"
    :description "Read a tweet and its replies. Pass the tweet ID (numeric)."
    :inputSchema {:type "object"
                  :properties {:tweet_id {:type "string"
                                          :description "The numeric tweet ID"}}
                  :required ["tweet_id"]}}
   {:name "post_tweet"
    :description "Post a new tweet. Requires being logged in to Twitter/X in Chrome."
    :inputSchema {:type "object"
                  :properties {:text {:type "string"
                                      :description "Tweet text (max 280 chars)"}}
                  :required ["text"]}}
   {:name "reply_tweet"
    :description "Reply to a tweet. Requires being logged in to Twitter/X in Chrome."
    :inputSchema {:type "object"
                  :properties {:tweet_id {:type "string"
                                          :description "The numeric tweet ID to reply to"}
                               :text {:type "string"
                                      :description "Reply text (max 280 chars)"}}
                  :required ["tweet_id" "text"]}}
   {:name "notifications"
    :description "Get your recent Twitter/X notifications (likes, replies, retweets, mentions)."
    :inputSchema {:type "object"
                  :properties {:n {:type "number"
                                   :description "Number of notifications (default 20)"}}}}
   {:name "follow"
    :description "Follow a Twitter/X user by their screen name."
    :inputSchema {:type "object"
                  :properties {:screen_name {:type "string"
                                             :description "Twitter handle (with or without @)"}}
                  :required ["screen_name"]}}])

(defn- respond [id result]
  {:jsonrpc "2.0" :id id :result result})

(defn- error-response [id code message]
  {:jsonrpc "2.0" :id id :error {:code code :message message}})

(defn- tool-result [text & {:keys [error?]}]
  {:content [{:type "text" :text text}]
   :isError (boolean error?)})

(defn- handle-initialize [id _params]
  (respond id
    {:protocolVersion "2024-11-05"
     :capabilities {:tools {}}
     :serverInfo {:name "twitter-mcp" :version "0.1.0"}
     :instructions "MCP server for Twitter/X. Uses Chrome cookies for authentication - make sure you're logged in to x.com in Chrome."}))

(defn- handle-tools-list [id _params]
  (respond id {:tools tools}))

(defn- clamp-n [args default]
  (let [raw (:n args)
        n (cond
            (nil? raw) default
            (number? raw) (int raw)
            (string? raw) (or (parse-long raw) default)
            :else default)]
    (min (max n 1) 50)))

(defn- handle-tools-call [id {:keys [name arguments]}]
  (try
    (let [result
          (case name
            "trending"
            (let [data (api/trending)]
              (fmt/format-trending data))

            "search"
            (let [data (api/search-tweets (:query arguments) (clamp-n arguments 20)
                         :sort (or (:sort arguments) "Top"))]
              (fmt/format-search-results data (str "Search: " (:query arguments))))

            "timeline"
            (let [data (api/home-timeline (clamp-n arguments 20))]
              (fmt/format-timeline data))

            "read_thread"
            (let [data (api/tweet-detail (:tweet_id arguments))]
              (fmt/format-thread data))

            "post_tweet"
            (let [data (api/create-tweet (:text arguments))]
              (fmt/format-create-result data))

            "reply_tweet"
            (let [data (api/create-tweet (:text arguments)
                         :reply-to-id (:tweet_id arguments))]
              (fmt/format-create-result data))

            "notifications"
            (let [data (api/notifications (clamp-n arguments 20))]
              (fmt/format-notifications data))

            "follow"
            (api/follow-user (:screen_name arguments))

            (throw (ex-info (str "Unknown tool: " name) {})))]
      (respond id (tool-result result)))
    (catch Exception e
      (respond id (tool-result (str "Error: " (.getMessage e)) :error? true)))))

(defn- handle-message [msg]
  (let [{:keys [id method params]} msg]
    (case method
      "initialize"                (handle-initialize id params)
      "notifications/initialized" nil
      "tools/list"                (handle-tools-list id params)
      "tools/call"                (handle-tools-call id params)
      "ping"                      (respond id {})
      ;; unknown
      (if id
        (error-response id -32601 (str "Method not found: " method))
        nil))))

(defn- write-response [resp]
  (let [out System/out
        line (str (json/write-str resp) "\n")]
    (.write out (.getBytes line "UTF-8"))
    (.flush out)))

(defn -main [& _args]
  (let [reader (BufferedReader. (InputStreamReader. System/in))]
    (loop []
      (when-let [line (.readLine reader)]
        (when-not (str/blank? line)
          (try
            (let [msg (json/read-str line :key-fn keyword)
                  resp (handle-message msg)]
              (when resp
                (write-response resp)))
            (catch Exception e
              (binding [*out* *err*]
                (println "Error processing message:" (.getMessage e))))))
        (recur)))))
