(ns twitter.format
  "Format Twitter API responses into readable markdown."
  (:require [clojure.string :as str]))

(defn- extract-tweet-result
  "Navigate the nested tweet result structure."
  [entry]
  (or (get-in entry [:content :itemContent :tweet_results :result])
      (get-in entry [:item :itemContent :tweet_results :result])
      entry))

(defn- unwrap-tweet
  "Unwrap tweet from possible tombstone or limited actions wrapper."
  [result]
  (cond
    (:tweet result) (:tweet result)
    (:legacy result) result
    :else result))

(defn- extract-user-info [tweet]
  "Try multiple paths to find user info in the nested structure."
  (or (get-in tweet [:core :user_results :result :legacy])
      (get-in tweet [:core :user_result :result :legacy])
      (get-in tweet [:user_results :result :legacy])
      {}))

(defn- format-single-tweet [result]
  (let [tweet (unwrap-tweet result)
        legacy (:legacy tweet)
        user (extract-user-info tweet)
        screen-name (or (:screen_name user)
                        (get-in tweet [:core :user_results :result :legacy :screen_name])
                        (get-in tweet [:core :user_results :result :rest_id])
                        "?")
        name (or (:name user) screen-name)
        text (or (:full_text legacy) (:text legacy)
                 (:full_text tweet) (:text tweet) "")
        likes (or (:favorite_count legacy) 0)
        rts (or (:retweet_count legacy) 0)
        replies (or (:reply_count legacy) 0)
        tweet-id (or (:rest_id tweet)
                     (:id_str legacy)
                     (:id_str tweet)
                     "?")
        created (or (:created_at legacy) (:created_at tweet) "")]
    (str "@" screen-name " (" name ")"
         (when (seq created) (str " | " created))
         "\n" text
         "\n" likes " likes | " rts " RTs | " replies " replies"
         " | https://x.com/" screen-name "/status/" tweet-id
         " [" tweet-id "]")))

(defn format-search-results
  "Format search timeline response."
  [data title]
  (let [instructions (get-in data [:data :search_by_raw_query :search_timeline :timeline :instructions])
        entries (->> instructions
                     (mapcat :entries)
                     (filter #(str/starts-with? (or (:entryId %) "") "tweet-")))]
    (if (seq entries)
      (str "# " title " (" (count entries) " tweets)\n\n"
           (str/join "\n\n---\n\n"
             (map-indexed
               (fn [i entry]
                 (try
                   (str (inc i) ". " (format-single-tweet (extract-tweet-result entry)))
                   (catch Exception e
                     (str (inc i) ". [error formatting tweet: " (.getMessage e) "]"))))
               entries)))
      (str "# " title "\n\nNo tweets found."))))

(defn format-timeline
  "Format home timeline response."
  [data]
  (let [instructions (get-in data [:data :home :home_timeline_urt :instructions])
        entries (->> instructions
                     (mapcat :entries)
                     (filter #(str/starts-with? (or (:entryId %) "") "tweet-")))]
    (if (seq entries)
      (str "# Home Timeline (" (count entries) " tweets)\n\n"
           (str/join "\n\n---\n\n"
             (map-indexed
               (fn [i entry]
                 (try
                   (str (inc i) ". " (format-single-tweet (extract-tweet-result entry)))
                   (catch Exception _
                     (str (inc i) ". [error formatting tweet]"))))
               entries)))
      "# Home Timeline\n\nNo tweets found.")))

(defn format-thread
  "Format tweet detail (thread) response."
  [data]
  (let [instructions (get-in data [:data :threaded_conversation_with_injections_v2 :instructions])
        entries (->> instructions
                     (mapcat :entries)
                     (filter #(or (str/starts-with? (or (:entryId %) "") "tweet-")
                                  (str/starts-with? (or (:entryId %) "") "conversationthread-"))))]
    (if (seq entries)
      (let [formatted
            (for [entry entries]
              (let [entry-id (or (:entryId entry) "")]
                (cond
                  ;; Single tweet entry
                  (str/starts-with? entry-id "tweet-")
                  (try
                    (format-single-tweet (extract-tweet-result entry))
                    (catch Exception _ nil))

                  ;; Conversation thread (replies)
                  (str/starts-with? entry-id "conversationthread-")
                  (let [items (get-in entry [:content :items])]
                    (str/join "\n\n"
                      (for [item items
                            :let [result (extract-tweet-result item)]
                            :when result]
                        (try
                          (str "  " (format-single-tweet result))
                          (catch Exception _ nil)))))

                  :else nil)))]
        (str "# Thread\n\n"
             (str/join "\n\n---\n\n" (remove nil? formatted))))
      "# Thread\n\nNo content found.")))

(defn format-trending
  "Format trending topics response."
  [data]
  (let [modules (get-in data [:timeline :body :generalTimeline :timeline :entries])
        ;; Extract trend items from the explore page
        trends (->> modules
                    (mapcat (fn [entry]
                              (let [items (get-in entry [:content :items])]
                                (when items
                                  (for [item items
                                        :let [trend (get-in item [:item :content :trend])]
                                        :when trend]
                                    trend)))))
                    (remove nil?))]
    (if (seq trends)
      (str "# Trending (" (count trends) " topics)\n\n"
           (str/join "\n"
             (map-indexed
               (fn [i {:keys [name trendMetadata]}]
                 (str (inc i) ". " name
                      (when-let [desc (:description trendMetadata)]
                        (str " - " desc))
                      (when-let [count (:tweetCount trendMetadata)]
                        (str " (" count " tweets)"))))
               trends)))
      ;; Fallback: try to extract from a different structure
      (let [all-trends (->> (tree-seq coll? seq data)
                            (filter #(and (map? %)
                                          (:name %)
                                          (or (:trendMetadata %)
                                              (:url %)))))]
        (if (seq all-trends)
          (str "# Trending (" (count all-trends) " topics)\n\n"
               (str/join "\n"
                 (map-indexed
                   (fn [i {:keys [name trendMetadata]}]
                     (str (inc i) ". " name
                          (when-let [count (:tweetCount trendMetadata)]
                            (str " (" count " tweets)"))))
                   all-trends)))
          "# Trending\n\nCould not extract trending topics.")))))

(defn format-create-result
  "Format create tweet response."
  [data]
  (let [result (get-in data [:data :create_tweet :tweet_results :result])
        tweet-id (get-in result [:rest_id])
        legacy (:legacy result)
        screen-name (get-in result [:core :user_results :result :legacy :screen_name])]
    (if tweet-id
      (str "Tweet posted successfully.\nhttps://x.com/" (or screen-name "i") "/status/" tweet-id)
      "Tweet posted (could not extract URL from response).")))

(defn format-notifications
  "Format notifications response from v2 REST endpoint."
  [data]
  (let [notifications (get-in data [:globalObjects :notifications])
        tweets (get-in data [:globalObjects :tweets])
        users (get-in data [:globalObjects :users])
        ;; Get timeline entries for ordering
        instructions (get-in data [:timeline :instructions])
        entries (->> instructions
                     (mapcat :addEntries)
                     (mapcat :entries)
                     (concat (->> instructions (mapcat :entries)))
                     (filter #(str/starts-with? (or (get-in % [:content :operation :cursor :entryIdToReplace])
                                                    (:entryId %)
                                                    "") "notification-")))]
    (if (and notifications (seq notifications))
      (let [notif-items
            (for [[nid notif] (take 20 (sort-by (fn [[_ n]] (- (or (parse-long (str (:timestampMs n))) 0))) notifications))]
              (let [message (get-in notif [:message :text])
                    tweet-ids (get-in notif [:template :aggregateUserActionsV1 :targetObjects])
                    first-tweet-id (some->> tweet-ids first :tweet :id)
                    tweet (when first-tweet-id (get tweets (keyword first-tweet-id)))
                    tweet-text (or (:full_text tweet) (:text tweet))
                    tweet-user-id (:user_id_str tweet)
                    tweet-user (when tweet-user-id (get users (keyword tweet-user-id)))
                    screen-name (:screen_name tweet-user)
                    ;; From users (who triggered the notification)
                    from-ids (get-in notif [:template :aggregateUserActionsV1 :fromUsers])
                    from-names (->> from-ids
                                    (map #(get-in % [:user :id]))
                                    (map #(when % (get users (keyword %))))
                                    (map :screen_name)
                                    (remove nil?)
                                    (take 3))
                    ts (:timestampMs notif)]
                (str (when (seq from-names)
                       (str "@" (str/join ", @" from-names) " | "))
                     (or message "notification") "\n"
                     (when (seq tweet-text)
                       (str "> " (subs tweet-text 0 (min 200 (count tweet-text))) "\n"))
                     (when (and screen-name first-tweet-id)
                       (str "https://x.com/" screen-name "/status/" first-tweet-id "\n")))))]
        (str "# Notifications (" (count notif-items) ")\n\n"
             (str/join "\n---\n\n"
               (map-indexed (fn [i text] (str (inc i) ". " text)) notif-items))))
      "# Notifications\n\nNo notifications found.")))
