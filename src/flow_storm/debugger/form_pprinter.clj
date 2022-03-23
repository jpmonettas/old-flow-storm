(ns flow-storm.debugger.form-pprinter
  (:require [clojure.pprint :as pp]
            [flow-storm.instrument.forms :refer [tag-form-recursively] :as inst-forms]))

(defn- seq-delims [form]
  (let [delims (pr-str (empty form))]
    (if (= (count delims) 2)
      [(str (first delims)) (str (second delims))]
      ["#{" "}"])))

(defn- form-tokens [form]
  (let [curr-coord (::inst-forms/coor (meta form))]
    (cond
      (or (seq? form) (vector? form) (set? form))
      (let [[db de] (seq-delims form)]
        (-> [[db curr-coord]]
            (into (mapcat (fn [i f] (form-tokens f)) (range) form))
            (into [[de curr-coord]])))

      (map? form)
      (let [keys-vals (mapcat identity form)
            keys-vals-tokens (mapcat (fn [i f] (form-tokens f))
                                     (range)
                                     keys-vals)]
        (-> [["{" curr-coord]]
            (into keys-vals-tokens)
            (into [["}" curr-coord]])))

      :else
      [[(pr-str form) curr-coord]])))

(defn- consecutive-inv-chars [inv-chars-map idx]
  (loop [i (inc idx)
         inv-chars [(inv-chars-map idx)]]
    (if-let [inv-char (inv-chars-map i)]
      (recur (inc i) (conj inv-chars inv-char))
      inv-chars)))

(defn pprint-tokens [form]
  (let [form (tag-form-recursively form)
        pprinted-str (with-out-str
                       (binding [pp/*print-pprint-dispatch* pp/code-dispatch]
                         (pp/pprint form)))
        pos->layout-char (->> pprinted-str
                              (keep-indexed (fn [i c] (cond
                                                        (= c \newline) [i :nl]
                                                        (= c \space)   [i :sp]
                                                        (= c \,)       [i :sp]
                                                        :else nil)))
                              (into {}))
        pre-tokens (form-tokens form)]
    (loop [[[tname :as tok] & next-tokens] pre-tokens
           i 0
           final-toks []]
      (if-not tok
        final-toks
        (if (pos->layout-char i)
          (let [consec-inv-chars (consecutive-inv-chars pos->layout-char i)]
            (recur next-tokens
                   (+ i (count tname) (count consec-inv-chars))
                   (-> final-toks
                       (into consec-inv-chars)
                       (into  [tok]))))
          (recur next-tokens (+ i (count tname)) (into final-toks [tok])))))))

(defn- debug-print-tokens [ptokens]
  (doseq [t ptokens]
    (cond
      (= :sp t) (print " ")
      (= :nl t) (println)
      :else     (print (first t))))
  (println))

(comment

  (let [test-form '(defn factorial [n] (if (zero? n) 1 (* n (factorial (dec n)))))]
    (binding [pp/*print-right-margin* 40
              pp/*print-pprint-dispatch* pp/code-dispatch]
      (= (-> test-form
             (pprint-tokens)
             debug-print-tokens
             with-out-str)
         (-> test-form
             pp/pprint
             with-out-str))))

  (def test-form '(defn load! []
                    (let [environment (or (get-env-variable "SPREAD_ENV") "dev")
                          dev-env?    (= "dev" environment)
                          {{:keys [client-secret]} :google
                           {:keys [api-key]}       :sendgrid
                           :keys                   [private-key]}
                          (when dev-env?
                            (try-secrets "secrets.edn"))]

                      {:env     environment
                       :version "1.0.3"
                       :logging {:level (or (keyword (get-env-variable "LOGGING_LEVEL")) :debug) :pretty? dev-env?}
                       :api     {:port            (Integer/parseInt (or (get-env-variable "API_PORT") "3001"))
                                 :host            (or (get-env-variable "API_HOST") "0.0.0.0")
                                 :allowed-origins #{"http://localhost:8020"
                                                    "http://127.0.0.1:8020"
                                                    "https://studio.apollographql.com"
                                                    "https://spreadviz.org"
                                                    "https://www.spreadviz.org"}}
                       :aws     (cond-> {:region (when-not dev-env?
                                                   (get-env-variable "API_AWS_REGION"))
                                         :access-key-id  (or (get-env-variable "API_AWS_ACCESS_KEY_ID") "AKIAIOSFODNN7EXAMPLE")
                                         :secret-access-key (or (get-env-variable "API_AWS_SECRET_ACCESS_KEY") "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY")
                                         :bucket-name (or (get-env-variable "BUCKET_NAME") "spread-dev-uploads")
                                         :workers-queue-url (or (get-env-variable "WORKERS_QUEUE_URL") "http://localhost:9324/queue/workers")}
                                  dev-env? (assoc :sqs-host "localhost"
                                                  :sqs-port 9324
                                                  :s3-host "127.0.0.1"
                                                  :s3-port 9000))
                       :db
                       {:dbname   (or (get-env-variable "DB_DATABASE") "spread")
                        :port     (or (get-env-variable "DB_PORT") 3306)
                        :user     (or (get-env-variable "DB_USER") "root")
                        :password (or (get-env-variable "DB_PASSWORD") "Pa55w0rd")
                        :host     (or (get-env-variable "DB_HOST") "127.0.0.1")}

                       :google
                       {:client-id     (or (get-env-variable "GOOGLE_CLIENT_ID") "806052757605-5sbubbk9ubj0tq95dp7b58v36tscqv1r.apps.googleusercontent.com")
                        :client-secret (or client-secret (get-env-variable "GOOGLE_CLIENT_SECRET"))}

                       :sendgrid
                       {:template-id "d-02dda5f4b9e94948aedcec04b0e37abc"
                        :api-key     (or (get-env-variable "SENDGRID_API_KEY") api-key)}

                       :public-key
                       (or (get-env-variable "PUBLIC_KEY")
                           "-----BEGIN PUBLIC KEY-----\nMFwwDQYJKoZIhvcNAQEBBQADSwAwSAJBAJliLjOIAqGbnjGBM1RJml/l0MHayaRH\ncgEg00O9wBYvoNXrstFSzKTCKtG5MayUKgdG7C/98nu/TEzhvRFjINcCAwEAAQ==\n-----END PUBLIC KEY-----\n")
                       :private-key (or private-key (get-env-variable "PRIVATE_KEY"))})))

  (binding [pp/*print-right-margin* 80
              pp/*print-pprint-dispatch* pp/code-dispatch]
      (= (-> test-form
             (pprint-tokens)
             debug-print-tokens)))

  )
