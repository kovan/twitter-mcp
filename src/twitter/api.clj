(ns twitter.api
  "Twitter internal API access using Chrome cookies and bearer token."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [twitter.cookies :as cookies])
  (:import [java.net URLEncoder]))

;; Public bearer token embedded in Twitter's web app JS (same for all users)
(def ^:private bearer-token
  "AAAAAAAAAAAAAAAAAAAAANRILgAAAAAAnNwIzUejRCOuH5E6I8xnZz4puTs=1Zv7ttfk8LF81IUq16cHjhLTvJu4FA33AGWWjCpTnA")

(def ^:private ua
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36")

;; --- State ---
(def ^:private cookie-jar (atom nil))
(def ^:private csrf-token (atom nil))
(def ^:private query-ids (atom {}))

;; --- Cookie init ---
(defn- init-cookies! []
  (when-not @cookie-jar
    (let [cks-x (try (cookies/get-cookies ".x.com") (catch Exception _ []))
          cks-tw (try (cookies/get-cookies ".twitter.com") (catch Exception _ []))
          all-cks (distinct (concat cks-x cks-tw))
          ct0 (->> all-cks (filter #(= (:name %) "ct0")) first :value)
          f (java.io.File/createTempFile "tw-cookies" ".txt")]
      (.deleteOnExit f)
      (with-open [w (io/writer f)]
        (.write w "# Netscape HTTP Cookie File\n")
        (doseq [{:keys [name value path host]} all-cks]
          (when (seq value)
            (.write w (str host "\t"
                          (if (str/starts-with? host ".") "TRUE" "FALSE") "\t"
                          (or path "/") "\t"
                          "TRUE\t"
                          "0\t"
                          name "\t"
                          value "\n")))))
      (reset! cookie-jar (.getAbsolutePath f))
      (when ct0 (reset! csrf-token ct0))
      (binding [*out* *err*]
        (println (str "Loaded " (count all-cks) " cookies"
                      (when ct0 " (csrf ok)")))))))

;; --- Raw HTTP ---
(defn- curl-raw
  "Unauthenticated curl. Returns {:status :body}."
  [url]
  (let [body-file (java.io.File/createTempFile "tw-raw" ".html")
        args ["curl" "-sSL"
              "-H" (str "User-Agent: " ua)
              "-o" (.getAbsolutePath body-file)
              "-w" "%{http_code}"
              url]
        pb (ProcessBuilder. ^java.util.List args)
        proc (.start pb)
        status-str (str/trim (slurp (.getInputStream proc)))
        _ (.waitFor proc)
        body (slurp body-file)]
    (.delete body-file)
    {:status (or (parse-long status-str) 0) :body body}))

(defn- curl-authed
  "Authenticated curl with bearer + csrf. Returns {:status :body :data}."
  [url & {:keys [method json-body form-body]}]
  (init-cookies!)
  (let [body-file (java.io.File/createTempFile "tw-api" ".json")
        args (cond-> ["curl" "-sSL"
                      "-b" @cookie-jar "-c" @cookie-jar
                      "-H" (str "Authorization: Bearer " bearer-token)
                      "-H" (str "x-csrf-token: " (or @csrf-token ""))
                      "-H" "x-twitter-active-user: yes"
                      "-H" "x-twitter-auth-type: OAuth2Session"
                      "-H" "x-twitter-client-language: en"
                      "-H" (str "User-Agent: " ua)
                      "-o" (.getAbsolutePath body-file)
                      "-w" "%{http_code}"]
               (= method :post)
               (into ["-X" "POST"])
               json-body
               (into ["-H" "Content-Type: application/json"
                      "-d" (json/write-str json-body)])
               form-body
               (into ["-H" "Content-Type: application/x-www-form-urlencoded"
                      "-d" form-body])
               true
               (conj url))
        pb (ProcessBuilder. ^java.util.List args)
        proc (.start pb)
        status-str (str/trim (slurp (.getInputStream proc)))
        err (str/trim (slurp (.getErrorStream proc)))
        exit (.waitFor proc)
        body (slurp body-file)]
    (.delete body-file)
    (when-not (zero? exit)
      (binding [*out* *err*]
        (println "curl error:" err)))
    {:status (or (parse-long status-str) 0)
     :body body
     :data (try (json/read-str body :key-fn keyword) (catch Exception _ nil))}))

;; --- Query ID extraction ---
(defn- extract-query-ids! []
  "Extract GraphQL query IDs from Twitter's JS bundles."
  (when (empty? @query-ids)
    (binding [*out* *err*] (println "Extracting Twitter GraphQL query IDs..."))
    (let [page (:body (curl-raw "https://x.com"))
          ;; Find JS bundle URLs
          script-urls (->> (re-seq #"https://abs\.twimg\.com/responsive-web/client-web[^\s\"']*\.js" page)
                           distinct)]
      (binding [*out* *err*]
        (println (str "Found " (count script-urls) " JS bundles")))
      ;; Fetch bundles and extract query IDs
      (doseq [script-url (take 15 script-urls)]
        (try
          (let [js (:body (curl-raw script-url))
                ;; Pattern: queryId:"xxx",operationName:"YYY"
                matches (re-seq #"queryId:\"([^\"]+)\",operationName:\"([^\"]+)\"" js)]
            (doseq [[_ qid op-name] matches]
              (swap! query-ids assoc op-name qid)))
          (catch Exception _ nil)))
      (binding [*out* *err*]
        (println (str "Extracted " (count @query-ids) " query IDs"))
        (doseq [op ["CreateTweet" "TweetDetail" "SearchTimeline"
                     "HomeTimeline" "ExploreTrending"]]
          (when-let [qid (get @query-ids op)]
            (println (str "  " op " -> " qid))))))))

(defn- get-query-id [operation-name]
  (extract-query-ids!)
  (or (get @query-ids operation-name)
      (throw (ex-info (str "Query ID not found for " operation-name
                           ". Available: " (str/join ", " (keys @query-ids)))
                      {}))))

;; --- Feature flags and field toggles (extracted from JS bundles) ---
(def ^:private op-metadata (atom {}))

(defn- extract-op-metadata!
  "Extract per-operation featureSwitches and fieldToggles from JS bundles."
  []
  (when (empty? @op-metadata)
    (let [page (:body (curl-raw "https://x.com"))
          script-urls (->> (re-seq #"https://abs\.twimg\.com/responsive-web/client-web[^\s\"']*\.js" page)
                           distinct)]
      (doseq [script-url (take 15 script-urls)]
        (try
          (let [js (:body (curl-raw script-url))
                ;; Match full module exports with metadata
                modules (re-seq #"e\.exports=\{queryId:\"([^\"]+)\",operationName:\"([^\"]+)\",operationType:\"[^\"]+\",metadata:\{featureSwitches:\[([^\]]*)\],fieldToggles:\[([^\]]*)\]" js)]
            (doseq [[_ qid op-name features-str toggles-str] modules]
              (let [features (->> (re-seq #"\"([^\"]+)\"" features-str)
                                 (map second)
                                 vec)
                    toggles (->> (re-seq #"\"([^\"]+)\"" toggles-str)
                                (map second)
                                vec)]
                (swap! op-metadata assoc op-name
                       {:features features :toggles toggles}))))
          (catch Exception _ nil)))
      (binding [*out* *err*]
        (println (str "Extracted metadata for " (count @op-metadata) " operations"))))))

(defn- features-for
  "Get feature flags dict for an operation. All set to true by default (Twitter expects them present)."
  [op-name]
  (extract-op-metadata!)
  (let [feature-names (get-in @op-metadata [op-name :features] [])]
    (into {} (map (fn [f] [f true]) feature-names))))

(defn- field-toggles-for
  "Get field toggles dict for an operation."
  [op-name]
  (extract-op-metadata!)
  (let [toggle-names (get-in @op-metadata [op-name :toggles] [])]
    (into {} (map (fn [t] [t false]) toggle-names))))

;; --- Public API functions ---

(defn- graphql-get
  "Make an authenticated GraphQL GET request with proper features."
  [op-name variables]
  (let [qid (get-query-id op-name)
        features (features-for op-name)
        toggles (field-toggles-for op-name)
        url (str "https://x.com/i/api/graphql/" qid "/" op-name
                 "?variables=" (URLEncoder/encode (json/write-str variables) "UTF-8")
                 "&features=" (URLEncoder/encode (json/write-str features) "UTF-8")
                 "&fieldToggles=" (URLEncoder/encode (json/write-str toggles) "UTF-8"))
        resp (curl-authed url)]
    (when (not= 200 (:status resp))
      (throw (ex-info (str op-name " failed: HTTP " (:status resp))
                      {:body (subs (:body resp) 0 (min 500 (count (:body resp))))})))
    (:data resp)))

(defn- graphql-post
  "Make an authenticated GraphQL POST request with proper features."
  [op-name variables]
  (let [qid (get-query-id op-name)
        features (features-for op-name)
        toggles (field-toggles-for op-name)
        body {"variables" variables
              "features" features
              "fieldToggles" toggles
              "queryId" qid}
        resp (curl-authed (str "https://x.com/i/api/graphql/" qid "/" op-name)
               :method :post
               :json-body body)]
    (when (not= 200 (:status resp))
      (throw (ex-info (str op-name " failed: HTTP " (:status resp))
                      {:body (subs (:body resp) 0 (min 500 (count (:body resp))))})))
    (:data resp)))

(defn search-tweets
  "Search tweets. Returns raw API response. Uses POST (GET returns 404)."
  [query n & {:keys [sort]}]
  (graphql-post "SearchTimeline"
    {"rawQuery" query
     "count" n
     "querySource" "typed_query"
     "product" (or sort "Top")}))

(defn home-timeline
  "Get home timeline. Returns raw API response."
  [n]
  (graphql-get "HomeTimeline"
    {"count" n
     "includePromotedContent" false
     "latestControlAvailable" true
     "requestContext" "launch"}))

(defn tweet-detail
  "Get a tweet and its reply thread. Returns raw API response."
  [tweet-id]
  (graphql-get "TweetDetail"
    {"focalTweetId" tweet-id
     "with_rux_injections" false
     "rankingMode" "Relevance"
     "includePromotedContent" false
     "withCommunity" true
     "withQuickPromoteEligibilityTweetFields" false
     "withBirdwatchNotes" true
     "withVoice" true}))

(defn create-tweet
  "Post a new tweet or reply. Returns raw API response."
  [text & {:keys [reply-to-id]}]
  (graphql-post "CreateTweet"
    (cond-> {"tweet_text" text
             "dark_request" false
             "media" {"media_entities" []
                      "possibly_sensitive" false}
             "semantic_annotation_ids" []}
      reply-to-id
      (assoc "reply"
             {"in_reply_to_tweet_id" reply-to-id
              "exclude_reply_user_ids" []}))))

(defn trending
  "Get trending topics. Returns raw API response."
  []
  (let [resp (curl-authed "https://x.com/i/api/2/guide.json?include_page_configuration=true&initial_tab_id=trending")]
    (when (not= 200 (:status resp))
      (throw (ex-info (str "Trending failed: HTTP " (:status resp)) {})))
    (:data resp)))

(defn follow-user
  "Follow a Twitter user by screen name."
  [screen-name]
  (let [sn (str/replace screen-name #"^@" "")
        resp (curl-authed "https://x.com/i/api/1.1/friendships/create.json"
               :method :post
               :form-body (str "screen_name=" (URLEncoder/encode sn "UTF-8") "&follow=true"))]
    (when (not= 200 (:status resp))
      (throw (ex-info (str "Follow failed: HTTP " (:status resp)) {})))
    (let [user (:data resp)]
      (str "Now following @" (or (:screen_name user) sn)
           (when-let [n (:name user)] (str " (" n ")"))))))

(defn notifications
  "Get recent notifications. Uses REST v2 endpoint (no GraphQL query ID needed)."
  [n]
  (let [resp (curl-authed (str "https://x.com/i/api/2/notifications/all.json"
                               "?include_profile_interstitial_type=1"
                               "&include_blocking=1"
                               "&include_blocked_by=1"
                               "&include_followed_by=1"
                               "&include_mute_edge=1"
                               "&include_can_dm=1"
                               "&skip_status=1"
                               "&count=" n))]
    (when (not= 200 (:status resp))
      (throw (ex-info (str "Notifications failed: HTTP " (:status resp)) {})))
    (:data resp)))
