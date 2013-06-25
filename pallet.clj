;;; Pallet project configuration file

(require
 '[pallet.crate.postfix-test
   :refer [live-test-spec]]
 '[pallet.crates.test-nodes :refer [node-specs]])

(defproject postfix-crate
  :provider node-specs                  ; supported pallet nodes
  :groups [(group-spec "postfix-runit-test"
             :extends [with-automated-admin-user
                       (live-test-spec {})]
             :roles #{:live-test :default :runit})])
