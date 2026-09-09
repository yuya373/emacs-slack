;;; slack-org-alert-test.el --- tests for alert capture -*- lexical-binding: t; -*-

(require 'ert)
(require 'eieio)
(require 'dash)
(require 's)
(require 'slack-org-alert)

(defclass slack-org-alert-test-team ()
  ((id :initarg :id :initform "T0")
   (name :initarg :name :initform "myteam")
   (domain :initarg :domain :initform "myteam")
   (token :initarg :token :initform "x"))
  "Fake team object for alert capture tests.")

(defun slack-org-test--count-todos (file)
  "Return the number of TODO headings captured in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (s-count-matches "\\* TODO" (buffer-string))))

(ert-deftest slack-org-test-alert-heading-permalink ()
  "With a connected team the heading stores a permalink link and the
`slack-org-alert-tag' tag survives custom heading formats."
  (let ((team (make-instance 'slack-org-alert-test-team))
        (info (list :title "myteam - #chan"
                    :message "hello world"
                    :data (list :team-id "T0" :room-id "C1"
                                :ts "1730182493.679269"))))
    (cl-letf (((symbol-function 'slack-team-find) (lambda (_) team)))
      (let ((heading (slack-org-alert--heading info)))
        (should (s-contains?
                 "[[emacs-slack:https://myteam.slack.com/archives/C1/p1730182493679269?thread_ts=1730182493.679269&cid=C1][hello world]]"
                 heading))
        (should (s-contains? "TODO myteam - #chan" heading))
        (should (s-contains? " :slack:" heading))))))

(ert-deftest slack-org-test-alert-link-unresolvable ()
  "Alerts without message identity (no :data) have no link."
  (let ((team (make-instance 'slack-org-alert-test-team))
        (info (list :title "message deleted" :message nil)))
    (cl-letf (((symbol-function 'slack-team-find) (lambda (_) team)))
      (should (null (slack-org-alert--link info))))))

(ert-deftest slack-org-test-alert-notifier-skips-and-dedups ()
  "The notifier captures alerts once, skips duplicate links and skips
alerts without a resolvable link."
  (let ((slack-org-alert-file (make-temp-file "slack-org-alert-test"))
        (team (make-instance 'slack-org-alert-test-team))
        (info (list :title "myteam - #chan"
                    :message "hello world"
                    :data (list :team-id "T0" :room-id "C1"
                                :ts "1730182493.679269"))))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'slack-team-find) (lambda (_) team)))
            (slack-org-alert--notifier info)
            (slack-org-alert--notifier info)
            (should (= 1 (slack-org-test--count-todos slack-org-alert-file)))
            ;; a different message (different link) is captured
            (slack-org-alert--notifier
             (list :title "myteam - #chan"
                   :message "other message"
                   :data (list :team-id "T0" :room-id "C1"
                               :ts "1730182493.680000")))
            (should (= 2 (slack-org-test--count-todos slack-org-alert-file)))
            ;; alerts without message identity are skipped
            (slack-org-alert--notifier (list :title "message deleted"))
            (should (= 2 (slack-org-test--count-todos slack-org-alert-file)))))
      (delete-file slack-org-alert-file))))

(ert-deftest slack-org-test-alert-setup-idempotent ()
  "`slack-org-alert-setup' registers the style exactly once."
  (let ((alert-user-configuration nil))
    (slack-org-alert-setup)
    (should (= 1 (length alert-user-configuration)))
    (should (eq 'slack-org-alert-style
                (nth 1 (car alert-user-configuration))))
    (slack-org-alert-setup)
    (should (= 1 (length alert-user-configuration)))))

;;; slack-org-alert-test.el ends here
