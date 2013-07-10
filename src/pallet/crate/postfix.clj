(ns pallet.crate.postfix
  "Install and configure postfix.

## Links
- http://www.postfix.org/
- http://www.seaglass.com/postfix/
- http://www.arschkrebs.de/postfix/
"
  (:require
   [clj-schema.schema :refer [constraints def-map-schema map-schema
                              optional-path predicate-schema seq-schema
                              sequence-of]]
   [clojure.string :as string]
   [clojure.tools.logging :refer [debugf]]
   [pallet.actions :refer [directory exec-checked-script package-manager
                           packages remote-file]]
   [pallet.api :as api :refer [plan-fn]]
   [pallet.contracts :refer [any-value check-spec]]
   [pallet.crate :refer [assoc-settings defplan get-settings]]
   [pallet.crate-install :as crate-install]
   [pallet.crate.service
    :refer [supervisor-config supervisor-config-map] :as service]
   [pallet.script.lib :refer [config-root exit file pid-root spool-root]]
   [pallet.stevedore :refer [fragment script]]
   [pallet.utils :refer [apply-map deep-merge maybe-update-in]]
   [pallet.version-dispatch :refer [defmethod-version-plan
                                    defmulti-version-plan]]))

;;; # Settings

;;; To understand postfix and domans, read
;;; http://workaround.org/ispmail/squeeze/postfix-domain-types

;;; ## Daemon Settings

;;; A schema for daemon lines in master.cf
;;; See http://www.postfix.org/master.5.html for the defined constraints
(defn zero-process-limit-daemons
  [{:keys [name process-limit] :as daemon}]
  (if (#{"cleanup" "bounce" "defer"} name)
    (zero? process-limit)
    true))

(defn single-process-limit-daemons
  [{:keys [name process-limit] :as daemon}]
  (if (#{"pickup" "qmgr"} name)
    (= 1 process-limit)
    true))

(defn privileged-daemons
  [{:keys [name unprivileged] :as daemon}]
  (if (#{"local" "pipe" "spawn" "virtual"} name)
    (not unprivileged)
    true))

(defn not-chroot-daemons
  [{:keys [name chroot] :as daemon}]
  (if (#{"local" "pipe" "spawn" "virtual"} name)
    (not chroot)
    true))

(defn wake-up-daemons
  [{:keys [name wake-up] :as daemon}]
  (if (#{"pickup" "qmgr" "flush"} name)
    wake-up
    true))

(def-map-schema daemon-schema
  (constraints zero-process-limit-daemons
               single-process-limit-daemons
               privileged-daemons
               not-chroot-daemons
               wake-up-daemons)
  [[:name] string?
   [:type] #{:inet :fifo :unix :pass}
   (optional-path [:private]) any-value
   (optional-path [:unprivileged]) any-value
   (optional-path [:chroot]) any-value
   (optional-path [:process-limit]) number?
   (optional-path [:wake-up]) [:or number? string?]
   [:command] string?])

(defmacro check-daemon
  [m]
  (check-spec m `daemon-schema &form))

(def default-daemons
  [{:name "smtp" :type :inet :private false :command "smtpd"}
   {:name "pickup" :type :fifo :private false
    :wake-up 60 :process-limit 1 :command "pickup"}
   {:name "cleanup" :type :unix :private false
    :process-limit 0 :command "cleanup"}
   {:name "qmgr" :type :fifo :private false :chroot false
    :wake-up 300 :process-limit 1 :command "qmgr"}
   {:name "tlsmgr" :type :unix
    :wake-up "1000?" :process-limit 1 :command "tlsmgr"}
   {:name "rewrite" :type :unix :command "trivial-rewrite"}
   {:name "bounce" :type :unix :process-limit 0 :command "bounce"}
   {:name "defer" :type :unix :process-limit 0 :command "bounce"}
   {:name "trace" :type :unix :process-limit 0 :command "bounce"}
   {:name "verify" :type :unix :process-limit 1 :command "verify"}
   {:name "flush" :type :unix
    :private false :wake-up "1000?" :process-limit 0 :command "flush"}
   {:name "proxymap" :type :unix :chroot false :command "proxymap"}
   {:name "proxywrite" :type :unix
    :chroot false :process-limit 1 :command "proxymap"}
   {:name "smtp" :type :unix :command "smtp"}
   {:name "relay" :type :unix :command "smtp"}
   {:name "showq" :type :unix :private false :command "showq"}
   {:name "error" :type :unix :command "error"}
   {:name "retry" :type :unix :command "error"}
   {:name "discard" :type :unix :command "discard"}
   {:name "local" :type :unix
    :unprivileged false :chroot false :command "local"}
   {:name "virtual" :type :unix
    :unprivileged false :chroot false :command "virtual"}
   {:name "lmtp" :type :unix :chroot false :command "lmtp"}
   {:name "anvil" :type :unix :process-limit 1 :command "anvil"}
   {:name "scache" :type :unix :process-limit 1 :command "scache"}])

(def tls-daemons
  ;; only used by postfix-tls
  [{:name "smtps" :type :inet :private false :chroot false
    :command "smtpd -o smtpd_tls_wrappermode=yes -o smtpd_sasl_auth_enable=yes"}
   {:name "587" :type :inet :private false :chroot false
    :command "smtpd -o smtpd_enforce_tls=yes -o smtpd_sasl_auth_enable=yes"}])


;;; ## Configuration Helpers

;;; See http://www.postfix.org/BASIC_CONFIGURATION_README.html
(defn my-origin
  "Domain for outgoing mail.
  http://www.postfix.org/postconf.5.html#myorigin"
  [domain-name]
  {:pre [((some-fn keyword? string?) domain-name)]}
  {:myorigin (cond
              (= :mydomain domain-name) "$mydomain"
              (= :myhostname domain-name) "$myhostname"
              :else domain-name)})

(defn my-destination
  "Domains to accept mail for.
  http://www.postfix.org/BASIC_CONFIGURATION_README.html#mydestination
  http://www.postfix.org/postconf.5.html#mydestination"
  [domains]
  {:pre [(seq domains)]}
  {:mydestination
   (map #(cond
          (= :mydomain %) "$mydomain"
          (= :myhostname %) "$myhostname"
          :else %)
        domains)})

(defn my-networks
  "Networks to accept mail from.
  http://www.postfix.org/BASIC_CONFIGURATION_README.html#relay_from
  http://www.postfix.org/postconf.5.html#mynetworks"
  [networks]
  {:mynetworks networks})

(defn my-networks-style
  "Specify the default networks.  One of :host, :subnet, :class.
  http://www.postfix.org/BASIC_CONFIGURATION_README.html#relay_from
  http://www.postfix.org/postconf.5.html#mynetworks_style"
  [network-style]
  {:mynetworks_style (name network-style)})

(defn relay-domains
  "Domains to relay to.
  http://www.postfix.org/BASIC_CONFIGURATION_README.html#relay_to
  http://www.postfix.org/postconf.5.html#relay_domains"
  [domains]
  {:relay_domains domains})

(defn relay-host
  "Host to relay to.
  http://www.postfix.org/BASIC_CONFIGURATION_README.html#relayhost
  http://www.postfix.org/postconf.5.html#relayhost"
  [host]
  {:relayhost host})

(defn notify-classes
  "Notifications to send to postmaster.

  Possible classes are :bounce, :2bounce, :delay, :policy, :protocol, :resource,
  :software.

  http://www.postfix.org/BASIC_CONFIGURATION_README.html#notify
  http://www.postfix.org/postconf.5.html#notify_classes"
  [classes]
  {:notify_classes (map name classes)})

(defn proxy-interfaces
  "Interfaces the server is proxied behind.
  http://www.postfix.org/BASIC_CONFIGURATION_README.html#proxy_interfaces
  http://www.postfix.org/postconf.5.html#proxy_interfaces"
  [interfaces]
  {:proxy_interfaces interfaces})

(defn my-hostname
  "Hostname of server (must be FQDN).
  http://www.postfix.org/BASIC_CONFIGURATION_README.html#myhostname
  http://www.postfix.org/postconf.5.html#myhostname"
  [hostname]
  {:myhostname hostname})


(defn inet-interfaces
  "All interfaces the server should listen on.
  http://www.postfix.org/BASIC_CONFIGURATION_README.html#inet_interfaces
  http://www.postfix.org/postconf.5.html#inet_interfaces"
  [interfaces]
  {:inet_interfaces interfaces})


(defn virtual-mailbox-domains
  "Domains with virtual mailboxes.
  http://workaround.org/ispmail/squeeze/postfix-domain-types"
  [domains]
  {:virtual_mailbox_domains domains})

(defn virtual-mailbox-maps
  "User maps from username @ virtual mailbox domain, to mailbox storage path.
  http://workaround.org/ispmail/squeeze/postfix-domain-types"
  [maps]
  {:virtual_mailbox_maps maps})

(defn virtual-transport
  "Specify a virtual delivery process - this maps to an entry in master.cf"
  [transport]
  {:virtual_transport transport})

(defn virtual-transport-recipient-limit
  "Specify recipient limit for virtual delivery process."
  [transport limit]
  {(keyword (str transport "_destination_recipient_limit")) limit})

(defn smtp_relay_restrictions
  [restrictions]
  {:smtp_relay_restrictions restrictions})

(defn smtp_recipient_restrictions
  [restrictions]
  {:smtp_recipient_restrictions restrictions})

(defn secondary-mx
  "Configuration for a secondary MX machine for a remote site.
  http://www.postfix.org/STANDARD_CONFIGURATION_README.html#backup"
  [domain public-ip]
  (merge
   (relay-domains [domain])))

(defn primary-mx
  [domain public-ip]
  (primary-mx domain public-ip))

(defn service-name
  "Return a service name for riemann."
  [{:keys [instance-id] :as options}]
  (str "postfix" (when instance-id (str "-" instance-id))))

(defn defaults
  [options]
  {:conf-root (fragment (file (config-root) "postfix"))
   :run-root (fragment (file (pid-root) "postfix"))
   :service-name (service-name options)
   :spool-root (fragment (file (spool-root) "postfix"))
   :supervisor :initd
   :master default-daemons
   :main {}
   :user "postfix"
   :owner "postfix"
   :group "postfix"})

(defn derived-defaults
  [{:keys [conf-root] :as settings}]
  (debugf "derived-defaults settings %s" settings)
  (->
   settings
   (update-in [:master-cf] #(or % (fragment (file ~conf-root "master.cf"))))
   (update-in [:main-cf] #(or % (fragment (file ~conf-root "main.cf"))))
   (update-in [:run-command] #(or % "/usr/lib/postfix/master"))))

(defn db-file [db-name {:keys [instance-id] :as options}]
  (let [{:keys [conf-root]} (get-settings :postfix options)]
    (fragment (file ~conf-root ~db-name))))


;;; ## Installation Settings

;;; At the moment we just have a single implementation of settings,
;;; but this is open-coded.
(defmulti-version-plan settings-map [version settings])

(defmethod-version-plan
    settings-map {:os :linux}
    [os os-version version settings]
  (cond
   (:install-strategy settings) settings
   :else  (assoc settings
            :install-strategy :packages
            :packages ["postfix"]
            ;; preseeds to get a vaguely sane default
            :preseeds
            [{:line "postfix postfix/mailname string localhost.com"}
             {:line
              "postfix postfix/main_mailer_type select \"Internet Site\""}])))

(defn check-script
  []
  (script
   (chain-or ("daemon_directory=/usr/lib/postfix"
              "command_directory=/usr/sbin"
              "config_directory=/etc/postfix"
              "data_directory=/var/lib/postfix"
              "queue_directory=/var/spool/postfix"
              "mail_owner=postfix"
              "setgid_group=postdrop"
              "/etc/postfix/postfix-script" check)
             (exit 1))))

(defmethod supervisor-config-map [:postfix :initd]
  [_ {:keys [service-name] :as settings} options]
  {:service-name service-name})

(defmethod supervisor-config-map [:postfix :runit]
  [_ {:keys [run-command service-name] :as settings} options]
  {:service-name service-name
   :run-file {:content (str "#!/bin/sh\nexec 1>&2\n"
                            (check-script) "\n"
                            "exec " run-command)}})

(defmethod supervisor-config-map [:postfix :upstart]
  [_ {:keys [run-command service-name] :as settings} options]
  {:service-name service-name
   :exec run-command})

(defmethod supervisor-config-map [:postfix :nohup]
  [_ {:keys [run-command service-name] :as settings} options]
  {:service-name service-name
   :run-file {:content run-command}})

(defplan settings
  "Apply settings for postfix."
  [{:keys [conf-root run-rot spool-root main master] :as settings}
   & {:keys [instance-id] :as options}]
  (debugf "settings 0 %s" settings)
  (let [settings (deep-merge (defaults settings) settings)
        _ (debugf "settings 1 %s" settings)
        settings (derived-defaults
                  (settings-map (:version settings) settings))]
    (debugf "settings %s" settings)
    (assoc-settings :postfix settings options)
    (supervisor-config :postfix settings (or options {}))))

;;; # Install
(defplan install
  "Install postfix."
  [{:keys [instance-id]}]
  (let [{:keys [install-strategy owner group log-dir] :as settings}
        (get-settings :postfix {:instance-id instance-id})]
    (crate-install/install :postfix instance-id)
    (when log-dir
      (directory log-dir :owner owner :group group :mode "0755"))))

;;; # Configuration

;;; TODO: Syslog config
;;; http://www.postfix.org/BASIC_CONFIGURATION_README.html#syslog_howto

;;; ## Configuration Formatters
(defn format-main
  "Format a configuration entry in main.cf."
  [[key value]]
  (debugf "format-main %s %s" key value)
  (assert ((some-fn keyword? string?) key))
  (str (name key) " = "
       (if (sequential? value)
         (string/join ", " value)
         value)))

(defn main-cf-content [main-settings]
  (debugf "main-cf-content %s" main-settings)
  (string/join \newline (map format-main main-settings)))

(defmulti database-config
  "Return a database config map for main.cf"
  (fn database-config-eval [type parameter value options] type))

(defmethod database-config :default
  [type parameter value options]
  {parameter (str (name type) ":" (db-file (name parameter) options))})

(defn database->config
  "Return a config map for the specified database settings"
  [databases options]
  (debugf "database->config %s" databases)
  (reduce
   (fn [m [{:keys [parameter type]} value]]
     (merge m (database-config type parameter value options)))
   {}
   databases))

(defn main-cf-file [main-cf-file main-settings]
  (debugf "main-cf-file %s %s" main-cf-file main-settings)
  (remote-file main-cf-file
               :content (main-cf-content main-settings)
               :literal true))


(defn format-daemon
  "Format a daemon configuration entry in master.cf."
  [{:keys [name type private unprivileged chroot wake-up process-limit command]
    :as daemon}]
  (check-daemon daemon)
  (let [bool-flag #(when (not (nil? %))
                     (if % "y" "n"))
        daemon (-> daemon
                   (update-in [:type] clojure.core/name)
                   (maybe-update-in [:private] bool-flag)
                   (maybe-update-in [:unprivileged] bool-flag)
                   (maybe-update-in [:chroot] bool-flag))]
    (string/join
     " "
     (map
      #(daemon % "-")
      [:name :type :private :unprivileged :chroot :wake-up :process-limit
       :command]))))

(defn master-cf [master-settings]
  (string/join \newline (map format-daemon master-settings)))

(defn master-cf-file [master-cf-file master-settings]
  (debugf "Settings %s %s" master-cf-file master-settings)
  (remote-file master-cf-file
               :content (master-cf master-settings)
               :literal true))


(defmulti database-config-file
  "Create database config files"
  (fn database-config-file-eval [type parameter value user options] type))

(defmethod database-config-file :default
  [type parameter value user options]
  (debugf "database-config-file %s %s" parameter value)
  (remote-file
   (db-file (name parameter) options)
   :content (main-cf-content
             (if (map? value) value (zipmap value (repeat 1))))
   :literal true
   :owner "root"
   :group "root")
  (exec-checked-script
   (str "Postscript Database File " (db-file (name parameter) options))
   ~(or (debugf "database-config-file %s %s" parameter value) "")
   ("postmap" ~(str (name type) ":" (db-file (name parameter) options)))))

(defn database->config-file
  [database user options]
  (doseq [[{:keys [parameter type]} value] database]
    (database-config-file type parameter value user options)))

(defn configure
  [{:keys [instance-id] :as options}]
  (let [{:keys [database main-cf master-cf main master user] :as settings}
        (get-settings :postfix options)]
    (debugf "Settings %s" settings)
    (master-cf-file master-cf master)
    (main-cf-file main-cf (merge main (database->config database options)))
    (database->config-file database user options)))

;;; # Run
(defplan service
  "Run the postfix service."
  [& {:keys [action if-flag if-stopped instance-id]
      :or {action :manage}
      :as options}]
  (let [{:keys [supervision-options] :as settings}
        (get-settings :postfix {:instance-id instance-id})]
    (service/service settings (merge supervision-options
                                     (dissoc options :instance-id)))))

;;; # Server Spec

(defn server-spec
  [settings & {:keys [instance-id] :as options}]
  (api/server-spec
   :phases {:settings (plan-fn
                        (apply-map pallet.crate.postfix/settings
                                   settings options))
            :install (plan-fn (install options))
            :configure (plan-fn (configure options))}
   :default-phases [:install :configure]))
