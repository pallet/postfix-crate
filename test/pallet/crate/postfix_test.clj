(ns pallet.crate.postfix-test
  (:require
   [clojure.tools.logging :refer [debugf]]
   [clojure.test :refer :all]
   [pallet.actions :refer [as-action package-manager]]
   [pallet.api :refer [plan-fn server-spec]]
   [pallet.build-actions :refer [build-actions]]
   [pallet.common.logging.logutils :refer [suppress-logging]]
   [pallet.crate :refer [target-node]]
   [pallet.crate.initd :as initd]
   [pallet.crate.network-service :refer [wait-for-port-listen]]
   [pallet.crate.postfix :as postfix]
   [pallet.node :refer [primary-ip]]
   [pallet.test-utils]
   [postal.core :refer [send-message]]))


;;; Ensure the defaults meet the spec
(deftest default-daemon-test
  (is (every? map? (map #(postfix/check-daemon %) postfix/default-daemons)))
  (is (every? map? (map #(postfix/check-daemon %) postfix/tls-daemons))))

(deftest daemon-schema-test
  (is (map? (postfix/check-daemon {:name "a" :type :inet :command ""})))
  (suppress-logging
   (is (thrown? Exception (postfix/check-daemon {:name "a" :type :init})))
   (is (thrown? Exception (postfix/check-daemon {:name "a" :command ""})))
   (is (thrown? Exception (postfix/check-daemon {:type :init :command ""})))
   (is (thrown? Exception
                (postfix/check-daemon {:name "a" :type :fred :command ""})))
   (is (thrown? Exception
                (postfix/check-daemon {:name "a" :type "inet" :command ""})))))

(deftest master-cf-test
  (is (= "a inet - - - - - cmd"
         (postfix/master-cf [{:name "a" :type :inet :command "cmd"}]))))

(deftest main-cf-test
  (is (= "myorigin = here"
         (postfix/main-cf-content {:myorigin "here"}))))

(deftest my-origin-test
  (is (= {:myorigin "$mydomain"}
         (postfix/my-origin :mydomain))))

(deftest invoke-test
  (is (build-actions {}
        (postfix/settings {:database {{:type :cdb :parameter :aliases}
                                      {"postmaster" "root"}}})
        (postfix/install {})
        (postfix/configure {}))))

(defn send-email
  []
  (debugf "Sending test message to %s" (primary-ip (target-node)))
  (let [r (send-message
           ^{:host (primary-ip (target-node))}
           {:from "fred@fred.com"
            :to "fred@fred.com"
            :subject "Hello"
            :body "Hello"})]
    (debugf "Sent test message")
    (when-not (zero? (:code r))
      (throw (ex-info "Failed to send" r))))
  (let [r (send-message
           ^{:host (primary-ip (target-node))}
           {:from "fred@fred.com"
            :to "fred@fred.com"
            :subject "Hello"
            :body "Hello"
            :ssl "yes"})]
    (debugf "Sent ssl test message")
    (when-not (zero? (:code r))
      (throw (ex-info "Failed to send" r)))))

(defn live-test-spec
  [settings & {:keys [instance-id] :as options}]
  (server-spec
   :extends [(postfix/server-spec
              {:config
               (merge
                (postfix/my-origin :mydomain)
                (postfix/my-destination :mydomain)
                (postfix/virtual-mailbox-domains ["fred.com"]))})]
   :phases {:install (plan-fn (package-manager :update))
            :test (plan-fn
                    (postfix/service :action :start)
                    (wait-for-port-listen 25)
                    (as-action
                     (send-email)))}
   :default-phases [:install :configure :test]))
