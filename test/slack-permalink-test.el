;;; slack-permalink-test.el --- tests for permalink round-trips -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest slack-test-permalink-to-info ()
  "A standard permalink parses to team-domain, room-id, ts and thread-ts."
  (should (equal
           (list :team-domain "clojurians"
                 :room-id "C099W16KZ"
                 :ts "1730182493.679269"
                 :thread-ts "1730182493.679269")
           (slack-permalink-to-info
            "https://clojurians.slack.com/archives/C099W16KZ/p1730182493679269?thread_ts=1730182493.679269&cid=C099W16KZ")))
  ;; without thread_ts the message is its own thread root
  (should (equal
           (list :team-domain "clojurians"
                 :room-id "C099W16KZ"
                 :ts "1730182493.679269"
                 :thread-ts "1730182493.679269")
           (slack-permalink-to-info
            "https://clojurians.slack.com/archives/C099W16KZ/p1730182493679269"))))

(ert-deftest slack-test-info-to-permalink ()
  "Permalinks are well-formed query strings in both thread variants."
  (should (equal
           "https://clojurians.slack.com/archives/C099W16KZ/p1730182493679269?thread_ts=1730182493.679269&cid=C099W16KZ"
           (slack-info-to-permalink
            (list :team-domain "clojurians"
                  :room-id "C099W16KZ"
                  :ts "1730182493.679269"
                  :thread-ts "1730182493.679269"))))
  ;; no thread-ts: a single ?cid query parameter (not a dangling &cid)
  (should (equal
           "https://clojurians.slack.com/archives/C099W16KZ/p1730182493679269?cid=C099W16KZ"
           (slack-info-to-permalink
            (list :team-domain "clojurians"
                  :room-id "C099W16KZ"
                  :ts "1730182493.679269"
                  :thread-ts nil)))))

(ert-deftest slack-test-permalink-round-trip ()
    "info -> permalink -> info preserves all fields."
    (let ((info (list :team-domain "clojurians"
                      :room-id "C099W16KZ"
                      :ts "1730182493.679269"
                      :thread-ts "1730182493.679269")))
      (should (equal info (slack-permalink-to-info
                           (slack-info-to-permalink info))))))

;;; slack-permalink-test.el ends here
